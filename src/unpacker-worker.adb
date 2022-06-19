with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Directories; use Ada; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;
with Interfaces; use Interfaces;

with Unpacker.Package_File; use Unpacker.Package_File;
with Unpacker.Extract; use Unpacker.Extract;
with Unpacker.Package_File.Types; use Unpacker.Package_File.Types;
with Unpacker.Util; use Unpacker.Util;
with Unpacker.Crypto; use Unpacker.Crypto;

package body Unpacker.Worker is
	-- Array of Extract Tasks
	type Extract_Task_Array is array (Positive range <>) of Extract_Task;
	Extract_Tasks : access Extract_Task_Array;

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
		for I of Extract_Tasks.all loop
			I.Setup (File_Name, BV);
		end loop;

		-- Loop over Entries in Array
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
						& (case EI.Name is
							when By_ID =>
								Hex_String (H.Package_ID) & "-" & Hex_String (Unsigned_16 (C)),
							when By_Reference =>
								Decimal_String (E.Reference),
							when By_Hex_Reference_LE =>
								Hex_String_LE (E.Reference))
						& "." & EI.Ext;
				begin
--					Put_Line (Path); -- TODO Debug

					if not Exists (Path) then
						-- Check Package Cache Status
						if not Is_Package_Cached then
							Cache_Package (File_Name);
						end if;

						-- Delegate Extract Task
						Outer : loop
							for I of Extract_Tasks.all loop
								select
									I.Run (Path, E);
									exit Outer;
								else
									null;
								end select;
							end loop;
							delay 0.05;
						end loop Outer;
					end if;
				end;
			end if;

			C := C + 1;
		end loop;

		-- Wait until all extraction tasks are complete
		for I of Extract_Tasks.all loop
			I.Done;
		end loop;

		-- Free Package if Cached
		if Is_Package_Cached then
			Free_Package;
		end if;
	exception
		when E : Directories.Use_Error | Stream_IO.Use_Error =>
			Put_Line (Standard_Error,
				"[Fatal Error] Extract master task was unable to create "
				& "or access file or directory: "
				& Exception_Message (E));
			return;
	end Extract;

	-- Create extract tasks
	procedure Create_Extract_Tasks (Count : Positive) is
	begin
		Extract_Tasks := new Extract_Task_Array (1 .. Count);
	end Create_Extract_Tasks;

	-- Primary unpacker function
	procedure Unpack (Stream : in Stream_Access;
		File : in out Stream_IO.File_Type;
		File_Name : in String;
		Output_Dir : in String)
	is
		-- Package Information
		H : constant Header := Read_Header (Stream, Base_Name (File_Name));
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
