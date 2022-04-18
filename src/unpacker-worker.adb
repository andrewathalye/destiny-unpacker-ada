with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Directories; use Ada.Directories;
with Interfaces.C; use Interfaces.C;
with Unchecked_Deallocation;

-- TODO: Fix compression issue with linoodle; use linoodle;

with Unpacker.Crypto; use Unpacker.Crypto;
with Unpacker.Package_File; use Unpacker.Package_File;

package body Unpacker.Worker is
	-- Common Buffer Type
	type Data_Array_Access is access Data_Array;
	procedure Free is new Unchecked_Deallocation (Object => Data_Array, Name => Data_Array_Access);

	-- Modular I/O types
	package Unsigned_16_IO is new Modular_IO (Num => Unsigned_16);
	package Unsigned_32_IO is new Modular_IO (Num => Unsigned_32);

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

		return O;
	end Hex_String;

	-- Print Decimal String for Unsigned_32
	function Decimal_String (U : Unsigned_32) return String is (Unsigned_32'Image (U) (2 .. Unsigned_32'Image (U)'Last));

	-- Determine file with same Package ID and different patch ID
	function Determine_Patch_Name (File_Name : String; Patch_ID : Unsigned_16) return String is (File_Name (File_Name'First .. File_Name'Last - 5) & Unsigned_16'Image (Patch_ID)(2 .. Unsigned_16'Image (Patch_ID)'Last) & ".pkg"); 

	-- Read data from entry into buffer
	-- Data_B must be initialised as a Data_Array of bounds (1 .. E.File_Size) and freed when no longer needed by the calling subprogram.
	function Extract_Entry (File_Name : String; E : Entry_Type; BV : Block_Vectors.Vector; Data_B : in out not null Data_Array_Access ) return Boolean is
		-- Constants
		-- TODO: Need floor function?
		BLOCK_SIZE : constant Unsigned_32 := 16#40000#; -- Static size of data block
		Block_Count : constant Unsigned_32 := Unsigned_32 ((E.Starting_Block_Offset + E.File_Size - 1) / BLOCK_SIZE );
		Last_Block_ID : Unsigned_32 := E.Starting_Block + Block_Count;

		-- Variables
		Current_Block_ID : Unsigned_32 := E.Starting_Block;
		Current_Block : Block := Block_Vectors.Element (BV, Natural (Current_Block_ID));
		Discard_Size : size_t := 0; -- Used for results of Decompress operation (not needed)
		Opened_Patch_ID : Unsigned_16 := Current_Block.Patch_ID;

		-- File and Stream
		In_F : Stream_IO.File_Type; -- Input File
		In_S : Stream_Access; -- Input Stream

		-- Buffers
		Decompress_B : aliased Data_Array (1 .. Positive (BLOCK_SIZE));
		Current_Buffer_Offset : Natural := 0;

	begin
		-- TODO Debug
		-- Put_Line ("[Debug] File size: " & Unsigned_32'Image (E.File_Size));
		-- Put_Line ("[Debug] Current block ID: " & Unsigned_32'Image (Current_Block_ID));
		-- Put_Line ("[Debug] Last block ID: " & Unsigned_32'Image (Last_Block_Id));

		-- Open first patch file
		Open (In_F, In_File, Determine_Patch_Name (File_Name, Current_Block.Patch_ID));
		In_S := Stream (In_F);

		while Current_Block_ID <= Last_Block_ID loop
			Current_Block := Block_Vectors.Element (BV, Natural (Current_Block_ID)); -- Load next block (dup for first block)

			declare
				Block_B : aliased Data_Array (1 .. Natural (Current_Block.Size));
				Decrypt_B : aliased Data_Array (1 .. Natural (Current_Block.Size));
			begin
				-- TODO Debug
--				Put_Line ("[Debug] Processing block " & Unsigned_32'Image (Current_Block_ID));
				
				-- Open correct file if block is in different patch ID
				if Current_Block.Patch_ID /= Opened_Patch_ID then
					-- TODO Debug
					Put_Line ("[Debug] Opening " & Determine_Patch_Name (File_Name, Current_Block.Patch_ID));

					Close (In_F);
					Open (In_F, In_File, Determine_Patch_Name (File_Name, Current_Block.Patch_ID));
					In_S := Stream (In_F);

					Opened_Patch_ID := Current_Block.Patch_ID;
				end if;

				Set_Index (In_F, Stream_IO.Positive_Count (Current_Block.Offset + 1));
				Data_Array'Read (In_S, Block_B);
				
				if Mode = d1 then
					if (Current_Block.Bit_Flag and 1) > 0 then
						-- Discard_Size := OodleLZ_Decompress (Block_B'Address, size_t (Current_Block.Size), Decompress_B'Address, size_t (BLOCK_SIZE), 0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
						Close (In_F); -- TODO Fix decompression
						return False; -- TODO Fix decompression
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
						-- Put_Line ("[Debug] Now decompressing " & Unsigned_32'Image (Current_Block_ID)); -- TODO Debug

						-- Put_Line ("[Debug] Current Block Size is: " & Unsigned_32'Image (Current_Block.Size)); -- TODO Debug

						-- Messy workaround for linoodle sigsegv TODO
						-- if not (Current_Block_ID = 14 and Current_Block.Size = 35726)
						--	and not (Current_Block_ID = 3168 and Current_Block.Size = 17930)
						--	and not (Current_Block_ID = 3279 and Current_Block.Size = 47312) then
						--		Discard_Size := OodleLZ_Decompress (Decrypt_B'Address, size_t (Current_Block.Size), Decompress_B'Address, size_t (BLOCK_SIZE), 0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
						--else
						--	Put_Line (Standard_Error, "[Error] Encountered block which would have crashed: " & Unsigned_32'Image (Current_Block_ID));
						-- end if;
--						Put_Line ("[Debug] Decompressed size is: " & size_t'Image (Discard_Size)); -- TODO Debug
--
						Close (In_F); -- TODO Fix decompression
						return False; -- TODO Fix decompression
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
						Current_Buffer_Offset := Current_Buffer_Offset + Natural (Copy_Size);
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

		Close (In_F); -- Make sure Patch file is closed before returning
		return True; 
	end Extract_Entry;

	-- Extract files
	procedure Extract (File_Name : in String; Output_Dir : in String; EV : in Entry_Vectors.Vector; BV : in Block_Vectors.Vector; H : in Header) is
		-- Constants necessary for extraction
		WEM_TYPE : constant Unsigned_8 := (case Mode is
			when prebl | postbl => 26,
			when d1 => 8);
		WEM_SUBTYPE : constant Unsigned_8 := (case Mode is
			when prebl => 6,
			when postbl => 7,
			when d1 => 21);
		BNK_TYPE : constant Unsigned_8 := (case Mode is
			when prebl | postbl => 26,
			when d1 => 0);
		BNK_SUBTYPE : constant Unsigned_8 := (case Mode is
			when prebl => 5,
			when postbl => 6,
			when d1 => 19);
		BNK_SUBTYPE_EXTRA : constant Unsigned_8 := 20; -- Only used for Destiny 1

		-- Variables
		C : Natural := 0; -- Count (acts as iterator)

		O : Stream_IO.File_Type; -- Output File
		OS : Stream_Access; -- Output Stream
	begin
		-- Loop over Entries in Vector
		for E of EV loop
			-- If entry is supported type, read data and write to file
			if E.Entry_Type = WEM_TYPE and E.Entry_Subtype = WEM_SUBTYPE then	
				declare
					Path : constant String := Output_Dir & "/wem/" & Decimal_String (E.Reference) & ".wem";
					Data_B : Data_Array_Access := new Data_Array (1 .. Natural (E.File_Size));
				begin
					-- Put_Line ("WEM: " & Decimal_String (E.Reference) & ".wem");
					if not Exists (Path) then -- Don't overwrite existing file
						-- Fill Buffer
						if Extract_Entry (File_Name, E, BV, Data_B) then -- Successfully extracted
							-- Write Data
							Create (O, Out_File, Path);
							OS := Stream (O);
							Data_Array'Write (OS, Data_B.all);
							Close (O);
							Free (Data_B);
						else
							Put_Line (Standard_Error, "[Decompress Error] Failed to extract output WEM " & Path);
						end if;
					end if;
				end;
			elsif E.Entry_Type = BNK_TYPE and (E.Entry_Subtype = BNK_SUBTYPE or (Mode = d1 and E.Entry_Subtype = BNK_SUBTYPE_EXTRA)) then
				declare
					Path : constant String := Output_Dir & "/bnk/" & Hex_String (H.Package_ID) & "-" & Hex_String (Unsigned_16 (C)) & ".bnk";
					Data_B : Data_Array_Access := new Data_Array (1 .. Natural (E.File_Size));
				begin
					-- Put_Line ("BNK: " & Hex_String (H.Package_ID) & "-" & Hex_String (Unsigned_16 (C)) & ".bnk");
					if not Exists (Path) then -- Save time and don't overwrite existing files
						-- Fill Buffer
						if Extract_Entry (File_Name, E, BV, Data_B) then -- Successfully extracted
							-- Write Data
							Create (O, Out_File, Path);
							OS := Stream (O);
							Data_Array'Write (OS, Data_B.all);
							Close (O);
							Free (Data_B);
						else
							Put_Line (Standard_Error, "[Decompress Error] Failed to extract output BNK " & Path);
						end if;
					end if;
				end;
			end if;
			
			C := C + 1; -- Increment iterator
		end loop;
	end Extract;

	-- Primary unpacker function
	procedure Unpack (Stream : in Stream_Access; File : in out Stream_IO.File_Type; File_Name : in String; Output_Dir : in String) is
		H : Header := Read_Header (Stream);
		E : Entry_Vectors.Vector;
		B : Block_Vectors.Vector;
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
