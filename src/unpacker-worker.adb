with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Directories; use Ada.Directories;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with Unchecked_Deallocation;

with linoodle; use linoodle;
with oodle; use oodle;

with Unpacker.Crypto; use Unpacker.Crypto;
with Unpacker.Package_File; use Unpacker.Package_File;

package body Unpacker.Worker is
	-- Common Buffer Type
	type Data_Array_Access is access Data_Array;

	-- Constant String Access
	type String_Access is access String;

	procedure Free is new Unchecked_Deallocation (Object => Data_Array, Name => Data_Array_Access);
	procedure Free is new Unchecked_Deallocation (Object => String, Name => String_Access);

	-- Modular I/O types
	package Unsigned_16_IO is new Modular_IO (Num => Unsigned_16);

	-- Print Hex String for Unsigned_16
	function Hex_String (Num : Unsigned_16) return String is
		S : String (1 .. 8); -- 16#XXXX#
		First : Natural := 0;
		Last : Natural := 0;
		O : String (1 .. 4) := "0000"; -- XXXX
		Pad : Natural := 0;
	begin
		Unsigned_16_IO.Put(S, Num, 16);

		-- First is first X, last last X in XXXX, XXX, XX, or X
		for I in S'Range loop
			if S (I) = '#' then
				if First = 0 then
					First := I + 1;
				else
					Last := I - 1;
				end if;
			end if;
		end loop;

		-- Fill in only used digits to ensure consistent length
		Pad := 3 - (Last - First);
		for I in First .. Last loop
			O (Pad + I - First + O'First) := S (I);
		end loop;

		return To_Lower (O);
	end Hex_String;

	-- Print Decimal String for Unsigned_32
	function Decimal_String (U : Unsigned_32) return String is (Unsigned_32'Image (U) (2 .. Unsigned_32'Image (U)'Last));

	-- Determine file with same Package ID and different patch ID
	function Determine_Patch_Name (File_Name : String; Patch_ID : Unsigned_16) return String is (File_Name (File_Name'First .. File_Name'Last - 5) & Unsigned_16'Image (Patch_ID)(2 .. Unsigned_16'Image (Patch_ID)'Last) & ".pkg"); 

	-- Read data from entry into buffer
	-- Data_B must be initialised as a Data_Array of bounds (1 .. E.File_Size) and freed when no longer needed by the calling subprogram.
	procedure Extract_Entry (In_F : Stream_IO.File_Type; File_Name : String; E : Entry_Type; BV : Block_Array; Data_B : not null Data_Array_Access ) is
		-- Constants
		-- TODO: Need floor function?
		BLOCK_SIZE : constant Unsigned_32 := 16#40000#; -- Static size of data block
		Block_Count : constant Unsigned_32 :=  (E.Starting_Block_Offset + E.File_Size - 1) / BLOCK_SIZE;
		Last_Block_ID : constant Unsigned_32 := E.Starting_Block + Block_Count;

		-- Variables
		Current_Block_ID : Unsigned_32 := E.Starting_Block;
		Current_Block : Block := BV (Natural (Current_Block_ID) + 1);
		Discard_Size : size_t := 0; -- Used for results of Decompress operation (not needed)
		Opened_Patch_ID : Unsigned_16 := Current_Block.Patch_ID;

		-- File and Stream
		Supplemental_File : Boolean := False; -- Does Supp_F need to be closed at end of subprogram
		Supp_F : Stream_IO.File_Type; -- Additional input File
		In_S : Stream_Access; -- Input Stream

		-- Buffers
		Decompress_B : aliased Data_Array (1 .. Positive (BLOCK_SIZE));
		Current_Buffer_Offset : Natural := 0;

	begin
		-- Open first patch file if not already open
		if File_Name /= Determine_Patch_Name (File_Name, Current_Block.Patch_ID) then
			Open (Supp_F, In_File, Determine_Patch_Name (File_Name, Current_Block.Patch_ID), "shared=no");
			In_S := Stream (Supp_F);
			Supplemental_File := True;
		else
			In_S := Stream (In_F);
		end if;

		while Current_Block_ID <= Last_Block_ID loop
			Current_Block := BV (Natural (Current_Block_ID) + 1); -- Load next block

			declare
				Block_B : aliased Data_Array (1 .. Natural (Current_Block.Size));
				Decrypt_B : aliased Data_Array (1 .. Natural (Current_Block.Size));
			begin
				-- Open correct file if block is in different patch ID
				if Current_Block.Patch_ID /= Opened_Patch_ID then
					Put_Line ("[Debug] Opening " & Determine_Patch_Name (File_Name, Current_Block.Patch_ID));

					-- If supplemental patch had already been opened
					if Supplemental_File then
						Close (Supp_F);
					end if;

					Open (Supp_F, In_File, Determine_Patch_Name (File_Name, Current_Block.Patch_ID), "shared=no");
					In_S := Stream (Supp_F);
					Supplemental_File := True; -- Mark for closing at end of subprogram

					Opened_Patch_ID := Current_Block.Patch_ID;
				end if;

				if Supplemental_File then
					Set_Index (Supp_F, Stream_IO.Positive_Count (Current_Block.Offset + 1));
				else
					Set_Index (In_F, Stream_IO.Positive_Count (Current_Block.Offset + 1));
				end if;

				Data_Array'Read (In_S, Block_B);
				
				if Mode = d1 then
					if (Current_Block.Bit_Flag and 1) > 0 then -- Always uses older LZ compression methods
						Discard_Size := LinoodleLZ_Decompress (Block_B'Address, size_t (Current_Block.Size), Decompress_B'Address, size_t (BLOCK_SIZE), 0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
					else
						Decompress_B := Block_B;	
					end if;
				else -- D2, so encryption possibly used
					if (Current_Block.Bit_Flag and 2) > 0 then	
						Decrypt_Block (Current_Block, Block_B, Decrypt_B);
					else
						Decrypt_B := Block_B;
					end if;

					if (Current_Block.Bit_Flag and 1) > 0 then
						if Mode = prebl then
							Discard_Size := LinoodleLZ_Decompress (Decrypt_B'Address, size_t (Current_Block.Size), Decompress_B'Address, size_t (BLOCK_SIZE), 0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
						else -- Post-BL, so newer compression methods
							Discard_Size := OodleLZ_Decompress (Decrypt_B'Address, size_t (Current_Block.Size), Decompress_B'Address, size_t (BLOCK_SIZE), 0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
						end if;
					else
						Decompress_B (Decrypt_B'Range) := Decrypt_B; -- Could be shorter in theory if last block
					end if;

				end if;

				-- TODO: Check array indices vs. C++
				-- If first block
				if Current_Block_ID = E.Starting_Block then
					declare
						Copy_Size : Natural;
					begin

						-- If also last block, file size is copy size
						if Current_Block_ID = Last_Block_ID then
							Copy_Size := Natural (E.File_Size);
						else
							Copy_Size := Natural (BLOCK_SIZE - E.Starting_Block_Offset);
						end if;

						Data_B (1 .. Copy_Size) := Decompress_B (Natural (E.Starting_Block_Offset) + 1 .. Natural (E.Starting_Block_Offset) + Copy_Size);	
						Current_Buffer_Offset := Current_Buffer_Offset + Copy_Size;
					end;
				-- If last block
				elsif Current_Block_ID = Last_Block_ID then
						Data_B (1 + Current_Buffer_Offset .. Natural (E.File_Size)) := Decompress_B (1 .. Natural (E.File_Size) - Current_Buffer_Offset);
				-- If normal block
				else
					Data_B (1 + Current_Buffer_Offset .. Current_Buffer_Offset + Natural (BLOCK_SIZE)) := Decompress_B (1 .. Natural (BLOCK_SIZE));
					Current_Buffer_Offset := Current_Buffer_Offset + Natural (BLOCK_SIZE);
				end if;
			end;

			Current_Block_ID := Current_Block_ID + 1;
		end loop;

		if Supplemental_File then
			Close (Supp_F); -- Make sure extra Patch file is closed before returning
		end if;
	end Extract_Entry;

	-- Task Definition for Parallel Extractions
	task type Extract_Task is
		entry Setup (File_Name : String; BV : Block_Array);
		entry Run (Path : String;  E : Entry_Type);
		entry Done;
	end Extract_Task;

	task body Extract_Task is
		-- Managed by Setup and Done
		Task_File_Name : String_Access;
		Task_BV : Block_Array_Access;
		In_F : Stream_IO.File_Type; -- Passed to Extract

		-- Managed by Run
		Task_Path : String_Access;
		Task_E : Entry_Type;

		-- Actual data buffer
		Data_B : Data_Array_Access;

		-- Output
		O : Stream_IO.File_Type;
		OS : Stream_Access;
	begin
		loop
			select
				-- Reduce copying for loop
				accept Setup (File_Name : String; BV : Block_Array) do
					Task_File_Name := new String'(File_Name);
					Task_BV := new Block_Array'(BV);
					Open (In_F, In_File, File_Name, "shared=no");
				end Setup;
			or
				accept Run (Path : String;  E : Entry_Type) do
					Task_Path := new String'(Path);
					Task_E := E;
				end Run;

				-- Fill Buffer
				Data_B := new Data_Array (1 .. Natural (Task_E.File_Size));
				Extract_Entry (In_F, Task_File_Name.all, Task_E, Task_BV.all, Data_B);	

				-- Write Data
				Create (O, Out_File, Task_Path.all);
				OS := Stream (O);
				Data_Array'Write (OS, Data_B.all);
				Close (O);

				-- Free Buffers and Strings
				Free (Data_B);
				Free (Task_Path);
			or
				accept Done do -- Clean up and free remaining items
					Free (Task_BV);
					Free (Task_File_Name);
					Close (In_F);
				end Done;
			or
				terminate;
			end select;
		end loop;
	end Extract_Task;

	-- Array of Extract Tasks
	MAX_TASKS : constant Positive := 11; -- TODO Add adjustment
	Extract_Tasks : array (1 .. MAX_TASKS) of Extract_Task;

	procedure Delegate_Extract_Task (P : String; E : Entry_Type) is
	begin
		Outer:
		loop
			for I of Extract_Tasks loop
				select
					I.Run (P, E);
					exit Outer;
				else
					null; -- If not available to start, keep checking
				end select;
			end loop;
			delay 0.05; -- Reduce idle looping
		end loop Outer;
	end Delegate_Extract_Task;

	-- Extract files
	procedure Extract (File_Name : String; Output_Dir : String; EV : Entry_Array; BV : Block_Array; H : Header) is
		

		-- Types
		type Name_Type is (BY_REF, BY_ID);

		-- Variables
		C : Natural := 0; -- Count (acts as iterator)
	begin
		-- Setup Extract Tasks with new File Name and Block Array
		for I of Extract_Tasks loop
			I.Setup (File_Name, BV);
		end loop;

		-- Loop over Entries in Vector
		for E of EV loop
			declare -- Defaults for output
				Subdir : String (1 .. 3) := "unk";
				Ext : String (1 .. 3) := "bin";
				Name : Name_Type := BY_ID;
				Should_Extract : Boolean := True;

				-- Convert internal Entry data to enums
				Entry_Reference : constant Entry_Reference_Type := To_Type (E.Reference);
				Entry_Type : constant Entry_Type_Type := To_Type (E.Entry_Type);
				Entry_Subtype : constant Entry_Subtype_Type := To_Type (E.Entry_Subtype);
			begin
				-- Check if entry is supported for extraction
				case Entry_Type is
					when THIRD_PARTY =>
						case Entry_Subtype is
							when WEM =>
								Name := BY_REF;
								Subdir := "wem";
								Ext := "wem";
							when BNK_IDX_BUF =>
								Subdir := "bnk";
								Ext := "bnk";
							when others =>
								Should_Extract := False;
						end case;
					when VIDEO =>
						case Entry_Subtype is
							when USM_TEXREF_DDS =>
								Subdir := "usm";
								Ext := "usm";
							when others =>
								Should_Extract := False;
						end case;
					when others =>
						case Entry_Reference is
							when STRING_BNK =>
								Subdir := "txt";
								Ext := "str";
							when STRING_REF =>
								Subdir := "txt";
								Ext := "ref";
							when others =>
								-- Put_Line ("[Debug] Unknown entry with ref " & Entry_Reference_Type'Image (Entry_Reference) & " type " & Entry_Type_Type'Image (Entry_Type) & " subtype " & Entry_Subtype_Type'Image (Entry_Subtype));
								Should_Extract := False;
						end case;
				end case;

				-- If should extract, read data and write to file
				if Should_Extract then
					declare
						Path : constant String := Output_Dir & "/" & Subdir & "/" & (if Name = BY_ID then Hex_String (H.Package_ID) & "-" & Hex_String (Unsigned_16 (C)) else Decimal_String (E.Reference)) & "." & Ext;
					begin
						Put_Line ("[Debug] Exporting " & Path); --TODO Debug
						if not Exists (Path) then
							Delegate_Extract_Task (Path, E);
						end if;
					end;
				end if;
			end;
			
			C := C + 1;
		end loop;

		-- Wait until all extraction tasks are complete
		for I of Extract_Tasks loop
			I.Done;
		end loop;
	end Extract;

	-- Primary unpacker function
	procedure Unpack (Stream : in Stream_Access; File : in out Stream_IO.File_Type; File_Name : in String; Output_Dir : in String) is
		H : constant Header := Read_Header (Stream);
		E : Entry_Array (1 .. Natural (H.Entry_Table_Size));
		B : aliased Block_Array (1 .. Natural (H.Block_Table_Size));
	begin
		-- TODO Debug
--		Put_Line ("[Debug] Header Dump: Package ID" & Unsigned_16'Image (H.Package_ID)
--			& " Build ID " & Unsigned_32'Image (H.Build_ID)
--			& " Patch ID " & Unsigned_16'Image (H.Patch_ID)
--			& " Entry Table Size " & Unsigned_32'Image (H.Entry_Table_Size)
--			& " Entry Table Offset " & Unsigned_32'Image (H.Entry_Table_Offset)
--			& " Block Table Size " & Unsigned_32'Image (H.Block_Table_Size)
--			& " Block Table Offset " & Unsigned_32'Image (H.Block_Table_Offset));
		-- TODO Debug

		Modify_Nonce (H);
		Read_Entries (Stream, File, E, H);
		Read_Blocks (Stream, File, B, H);
		Close (File); -- No longer needed directly
		Extract (File_Name, Output_Dir, E, B, H);
	end Unpack;
end Unpacker.Worker;
