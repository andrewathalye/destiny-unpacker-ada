with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Directories; use Ada; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;

with linoodle; use linoodle;
with oodle; use oodle;

with Unpacker.Crypto; use Unpacker.Crypto;
with Unpacker.Package_File; use Unpacker.Package_File;
with Unpacker.Package_File.Types; use Unpacker.Package_File.Types;
with Unpacker.Util; use Unpacker.Util;

package body Unpacker.Worker is
	-- Exceptions
	Extract_Exception : exception;
		-- Any error which prevents extraction of an entry
	Extract_Task_Exception : exception;
		-- Fatal error which forces task to terminate

	-- Read data from entry into buffer
	-- Data_B must be initialised as a Data_Array of bounds (1 .. E.File_Size) and
	-- managed by the calling subprogram
	procedure Extract_Entry (In_F : Stream_IO.File_Type; File_Name : String;
		E : Entry_Type; BV : Block_Array; Data_B : not null Data_Array_Access)
	is
		-- Constants
		BLOCK_SIZE : constant Unsigned_32 := 16#40000#; -- Static size of data block
		Block_Count : constant Unsigned_32 :=
			(E.Starting_Block_Offset + E.File_Size - 1) / BLOCK_SIZE;
		Last_Block_ID : constant Unsigned_32 := E.Starting_Block + Block_Count;

		-- Variables
		Current_Block_ID : Unsigned_32 := E.Starting_Block;
		Current_Block : Block := BV (Natural (Current_Block_ID) + 1);
		Discard_Size : size_t := 0;
			-- Used for results of Decompress operation (not needed)
		Opened_Patch_ID : Unsigned_16 := Current_Block.Patch_ID;

		-- File and Stream
		Supplemental_File : Boolean := False;
			-- Does Supp_F need to be closed at end of subprogram
		Supp_F : Stream_IO.File_Type; -- Additional input File
		In_S : Stream_Access; -- Input Stream

		-- Buffers
		Decompress_B : aliased Data_Array (1 .. Positive (BLOCK_SIZE));
		Current_Buffer_Offset : Natural := 0;

	begin
		-- Open first patch file if not already open
		if File_Name /= Determine_Patch_Name (File_Name, Current_Block.Patch_ID) then
			-- If patch file does not exist, exit.
			if not Exists
				(Determine_Patch_Name (File_Name, Current_Block.Patch_ID))
			then
				raise Extract_Exception with "Missing initial patch file "
					& Determine_Patch_Name (File_Name, Current_Block.Patch_ID);
			end if;

			Open (Supp_F,
				Mode => In_File,
				Name => Determine_Patch_Name (File_Name, Current_Block.Patch_ID),
				Form => "shared=no");
			In_S := Stream (Supp_F);
			Supplemental_File := True;
		else
			In_S := Stream (In_F);
		end if;

		while Current_Block_ID <= Last_Block_ID loop
			Current_Block := BV (Natural (Current_Block_ID) + 1); -- Load next block

			-- If block size invalid, exit
			if Current_Block.Size < 1 then
				raise Extract_Exception with "Invalid block size.";
			end if;

			declare
				Block_B : aliased Data_Array (1 .. Natural (Current_Block.Size));
				Decrypt_B : aliased Data_Array (1 .. Natural (Current_Block.Size));
			begin
				-- Open correct file if block is in different patch ID
				if Current_Block.Patch_ID /= Opened_Patch_ID then
					-- If supplemental patch had already been opened
					if Supplemental_File then
						Close (Supp_F);
					end if;

					-- If supplemental patch file does not exist, exit.
					if not Exists
						(Determine_Patch_Name (File_Name, Current_Block.Patch_ID))
					then
						raise Extract_Exception with "Missing supplemental patch file "
							& Determine_Patch_Name (File_Name, Current_Block.Patch_ID);
					end if;

					Open (Supp_F,
						Mode => In_File,
						Name => Determine_Patch_Name (File_Name, Current_Block.Patch_ID),
						Form => "shared=no");

					In_S := Stream (Supp_F);
					Supplemental_File := True; -- Mark for closing at end of subprogram

					Opened_Patch_ID := Current_Block.Patch_ID;
				end if;

				-- Increment stream index
				if Supplemental_File then
					Set_Index (Supp_F, Stream_IO.Positive_Count (Current_Block.Offset + 1));
				else
					Set_Index (In_F, Stream_IO.Positive_Count (Current_Block.Offset + 1));
				end if;

				-- Read raw data
				Data_Array'Read (In_S, Block_B);

				case Mode is
					when d1 | d1be => -- No encryption, old compression
						if (Current_Block.Bit_Flag and 1) > 0 then
							-- Always uses older LZ compression methods
							Discard_Size := LinoodleLZ_Decompress (Block_B'Address,
								size_t (Current_Block.Size),
								Decompress_B'Address,
								size_t (BLOCK_SIZE),
								0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
						else
							Decompress_B (Block_B'Range) := Block_B;
								-- Could be shorter in theory if last block
						end if;
					when prebl | postbl => -- Encryption, mixed compression
						-- If current block encrypted
						if (Current_Block.Bit_Flag and 2) > 0 then
							Decrypt_Block (Current_Block, Block_B, Decrypt_B);
						else -- Otherwise transfer data
							Decrypt_B := Block_B;
						end if;

						-- If current block compressed
						if (Current_Block.Bit_Flag and 1) > 0 then
							if Mode = prebl then -- Pre-BL, old compression
								Discard_Size := LinoodleLZ_Decompress (Decrypt_B'Address,
									size_t (Current_Block.Size),
									Decompress_B'Address,
									size_t (BLOCK_SIZE),
									0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
							else -- Post-BL, so newer compression methods
								Discard_Size := OodleLZ_Decompress (Decrypt_B'Address,
									size_t (Current_Block.Size),
									Decompress_B'Address,
									size_t (BLOCK_SIZE),
									0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
							end if;
						else -- Otherwise transfer data
							Decompress_B (Decrypt_B'Range) := Decrypt_B;
								-- Could be shorter in theory if last block
						end if;
				end case;

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

						Data_B (1 .. Copy_Size) :=
							Decompress_B (
								Natural (E.Starting_Block_Offset) + 1
								.. Natural (E.Starting_Block_Offset) + Copy_Size);
						Current_Buffer_Offset := Current_Buffer_Offset + Copy_Size;
					end;
				-- If last block
				elsif Current_Block_ID = Last_Block_ID then
						Data_B (1 + Current_Buffer_Offset .. Natural (E.File_Size)) :=
							Decompress_B (1 .. Natural (E.File_Size) - Current_Buffer_Offset);
				-- If normal block
				else
					Data_B (1 + Current_Buffer_Offset
						.. Current_Buffer_Offset + Natural (BLOCK_SIZE)) :=
							Decompress_B (1 .. Natural (BLOCK_SIZE));
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

				begin
					Extract_Entry (In_F, Task_File_Name.all, Task_E, Task_BV.all, Data_B);

					-- Write Data
					Create (O, Out_File, Task_Path.all);
					OS := Stream (O);
					Data_Array'Write (OS, Data_B.all);
					Close (O);

--					Put_Line ("[Debug] Extracted file: " & Task_Path.all); -- TODO Debug
				exception
					when E : Extract_Exception =>
						Put_Line (Standard_Error,
							"[Error] Failed to extract file "
							& Task_Path.all
							& ": "
							& Exception_Message (E));
				end;

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
	exception
		when E : Constraint_Error =>
			Put_Line (Standard_Error,
				"[Fatal Error] Extract task failed due to invalid bounds: "
				& Exception_Message (E));
			raise Extract_Task_Exception;
		when E : others =>
			Put_Line (Standard_Error,
				"[Unknown Error] "
				& Exception_Message (E));
			raise Extract_Task_Exception;
	end Extract_Task;

	-- Array of Extract Tasks
	MAX_TASKS : constant Positive := 11; -- TODO Add adjustment
	Extract_Tasks : array (1 .. MAX_TASKS) of Extract_Task;

	-- Invoke free extract task from array
	procedure Delegate_Extract_Task (P : String; E : Entry_Type) is
	begin
		Outer :
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
	procedure Extract (File_Name : String;
		Output_Dir : String;
		EV : Entry_Array;
		BV : Block_Array;
		H : Header;
		Language_ID : String)
	is
		-- Variables
		C : Natural := 0; -- Count (acts as iterator)
		EI : Entry_Info_Type;
	begin
		-- Setup Extract Tasks with new File Name and Block Array
		for I of Extract_Tasks loop
			I.Setup (File_Name, BV);
		end loop;

		-- Loop over Entries in Vector
		for E of EV loop
			-- Get information about current Entry
			EI := Get_Info (E, Language_ID);

			-- If should extract, read data and write to file
			if EI.Should_Extract then
				-- Create output directory if necessary
				if not Exists (Output_Dir & "/" & EI.Subdir & "/") then
					Create_Directory (Output_Dir & "/" & EI.Subdir & "/");
				end if;

				-- Create language-specific output directory if necessary
				if not Exists (Output_Dir & "/" & EI.Subdir & "/" & Language_ID) then
					Create_Directory (Output_Dir & "/" & EI.Subdir & "/" & Language_ID);
				end if;

				declare
					Path : constant String := Output_Dir & "/"
						& EI.Subdir & "/"
						& Language_ID & "/"
						& (if EI.Name = By_ID then Hex_String (H.Package_ID)
						& "-" & Hex_String (Unsigned_16 (C)) else Decimal_String (E.Reference))
						& "." & EI.Ext;
				begin
--					Put_Line (Path); -- TODO Debug
					if not Exists (Path) then
						Delegate_Extract_Task (Path, E);
					end if;
				end;
			end if;

			C := C + 1;
		end loop;

		-- Wait until all extraction tasks are complete
		for I of Extract_Tasks loop
			I.Done;
		end loop;
	exception
		when E : Directories.Use_Error | Stream_IO.Use_Error =>
			Put_Line (Standard_Error,
				"[Fatal Error] Extract master task was unable to create "
				& "or access file or directory: "
				& Exception_Message (E));
			return;
	end Extract;

	-- Primary unpacker function
	procedure Unpack (Stream : in Stream_Access;
		File : in out Stream_IO.File_Type;
		File_Name : in String;
		Output_Dir : in String)
	is
		-- Package Information
		H : constant Header := Read_Header (Stream);
		E : Entry_Array (1 .. Natural (H.Entry_Table_Size));
		B : aliased Block_Array (1 .. Natural (H.Block_Table_Size));
	begin
		Modify_Nonce (H); -- Only needed for Destiny 2
		Read_Entries (Stream, File, E, H);
		Read_Blocks (Stream, File, B, H);
		Close (File); -- No longer needed directly
		Extract (File_Name,
			Output_Dir,
			EV => E,
			BV => B,
			H => H,
			Language_ID => Get_Language_ID (File_Name));
	end Unpack;
end Unpacker.Worker;
