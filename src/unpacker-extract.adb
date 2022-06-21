with Interfaces.C; use Interfaces; use Interfaces.C;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Directories; use Ada.Directories;

with Config; use Config; -- Platform-specific configuration

with Unpacker.Crypto; use Unpacker.Crypto;
with Unpacker.Util; use Unpacker.Util;

package body Unpacker.Extract is
	-- Package Data Buffer
	Package_B : Data_Array_Access := null;

	-- Cache and Free Package Data
	-- See specification for exact details
	procedure Cache_Package (File_Name : String) is
		In_F : Stream_IO.File_Type;
		In_S : Stream_Access;
	begin
		if Package_B = null then
			Open (In_F, In_File, File_Name);
			In_S := Stream (In_F);
			Package_B := new Data_Array (1 .. Natural (Size (File_Name)));
			Data_Array'Read (In_S, Package_B.all);
			Close (In_F);
		else
			raise Package_Cache_Exception;
		end if;
	end Cache_Package;

	function Is_Package_Cached return Boolean is (Package_B /= null);

	procedure Free_Package is
	begin
		if Package_B /= null then
			Free (Package_B);
			Package_B := null;
		else
			raise Package_Cache_Exception;
		end if;
	end Free_Package;

	-- Read data from entry into buffer
	-- Data_B must be initialised as a Data_Array of bounds (1 .. E.File_Size) and
	-- managed by the calling subprogram

	procedure Extract_Entry (File_Name : String; E : Entry_Type; BV : Block_Array;
		Data_B : not null Data_Array_Access)
	is
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
		Supp_S : Stream_Access; -- Input Stream

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
			Supp_S := Stream (Supp_F);
			Supplemental_File := True;
		end if;

		-- Check package cache validity
		if not Is_Package_Cached then
			raise Package_Cache_Exception;
		end if;

		while Current_Block_ID <= E.Last_Block loop
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

					-- If new patch ID is the cached patch ID, use that instead
					if File_Name = Determine_Patch_Name
						(File_Name, Current_Block.Patch_ID)
					then
						Supplemental_File := False;
					else

						Open (Supp_F,
							Mode => In_File,
							Name => Determine_Patch_Name (File_Name, Current_Block.Patch_ID),
							Form => "shared=no");

						Supp_S := Stream (Supp_F);
						Supplemental_File := True; -- Mark for closing at end of subprogram
					end if;

					Opened_Patch_ID := Current_Block.Patch_ID;
				end if;

				-- Increment stream index if using supplemental file
				if Supplemental_File then
					Set_Index (Supp_F, Stream_IO.Positive_Count (Current_Block.Offset + 1));
					-- Read raw data
					Data_Array'Read (Supp_S, Block_B);
				else -- Read data from memory buffer
					Block_B (1 .. Natural (Current_Block.Size)) :=
						Package_B (Natural (Current_Block.Offset + 1) ..
						Natural (Current_Block.Offset + Current_Block.Size));
				end if;

				-- Handle encrypted blocks
				case Current_Block.Encryption is
					when None =>
						Decrypt_B := Block_B;
					when others =>
						Decrypt_Block (Current_Block, Block_B, Decrypt_B);
				end case;

				-- Handle compressed blocks
				-- The actual function called is determined by config.ads
				case Current_Block.Compression is
					when None =>
						Decompress_B (Decrypt_B'Range) := Decrypt_B;
							-- Can be shorter if last block
					when Old_Type => -- LZH, oftentimes. Requires old Oodle.
						Discard_Size := Config_Old_OodleLZ_Decompress (Decrypt_B'Address,
							size_t (Current_Block.Size),
							Decompress_B'Address,
							size_t (BLOCK_SIZE),
							0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
					when New_Type  => -- Unknown, uses new Oodle.
						Discard_Size := Config_New_OodleLZ_Decompress (Decrypt_B'Address,
							size_t (Current_Block.Size),
							Decompress_B'Address,
							size_t (BLOCK_SIZE),
							0, 0, 0, 0, 0, 0, 0, 0, 0, 3);
				end case;

				-- Copy the required data - this depends on the block position
				if Current_Block_ID = E.Starting_Block then
					declare
						Copy_Size : Natural;
					begin

						-- If also last block, file size is copy size
						if Current_Block_ID = E.Last_Block then
							Copy_Size := Natural (E.File_Size);
						else
							Copy_Size := Natural (BLOCK_SIZE - E.Starting_Block_Offset);
						end if;

						Data_B (1 .. Copy_Size) :=
							Decompress_B (
								Natural (E.Starting_Block_Offset) + 1
								.. Natural (E.Starting_Block_Offset) + Copy_Size);
						Current_Buffer_Offset := @ + Copy_Size;
					end;
				elsif Current_Block_ID = E.Last_Block then
						Data_B (1 + Current_Buffer_Offset .. Natural (E.File_Size)) :=
							Decompress_B (1 .. Natural (E.File_Size) - Current_Buffer_Offset);
				else
					Data_B (1 + Current_Buffer_Offset
						.. Current_Buffer_Offset + Natural (BLOCK_SIZE)) :=
							Decompress_B (1 .. Natural (BLOCK_SIZE));
					Current_Buffer_Offset := @ + Natural (BLOCK_SIZE);
				end if;
			end;

			Current_Block_ID := @ + 1;
		end loop;

		if Supplemental_File then
			Close (Supp_F); -- Make sure extra Patch file is closed before returning
		end if;
	end Extract_Entry;

	-- Task Implementation for Parallel Extractions
	task body Extract_Task is
		-- Managed by Setup and Done
		Task_File_Name : String_Access;
		Task_BV : Block_Array_Access;

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
				end Setup;
			or
				accept Run (Path : String; E : Entry_Type) do
					Task_Path := new String'(Path);
					Task_E := E;
				end Run;

				-- Fill Buffer
				Data_B := new Data_Array (1 .. Natural (Task_E.File_Size));

				begin
					Extract_Entry (Task_File_Name.all, Task_E, Task_BV.all, Data_B);

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
end Unpacker.Extract;
