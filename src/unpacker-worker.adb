with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;
with Ada.Directories; use Ada; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;

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
		Entry_ID : Natural := 0; -- Entry ID Iterator
		EI : Entry_Info_Type;
	begin
		-- Setup Extract Tasks with new File Name and Block Array
		for I of Extract_Tasks.all loop
			I.Setup (File_Name, BV);
		end loop;

		-- Loop over Entries in Array
		Process_Entries :
		for E of EV loop
			-- Get information about current Entry
			EI := Get_Info (E, Language_ID);

			-- If should extract, read data and write to file
			if EI.Should_Extract then
				-- Create language-specific output directory if necessary
				if not Exists (Output_Dir & "/" & EI.Subdir & "/" & Language_ID) then
					-- Create output subdirectory if necessary
					if not Exists (Output_Dir & "/" & EI.Subdir & "/") then
						-- Create output directory if necessary
						if not Exists (Output_Dir) then
							Create_Directory (Output_Dir);
						end if;

						Create_Directory (Output_Dir & "/" & EI.Subdir & "/");
					end if;

					Create_Directory (Output_Dir & "/" & EI.Subdir & "/" & Language_ID);
				end if;

				declare
					Path : constant String := Output_Dir & "/"
						& EI.Subdir & "/"
						& Language_ID & "/"
						& (case EI.Name is
							when By_ID =>
								Hex_String (H.Package_ID) & "-" & Hex_String (Unsigned_16 (Entry_ID)),
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

			Entry_ID := @ + 1;
		end loop Process_Entries;

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
		EV : Entry_Array (1 .. Natural (H.Entry_Table_Size));
		BV : Block_Array (1 .. Natural (H.Block_Table_Size));
	begin
		Modify_Nonce (H); -- Only needed for Destiny 2
		Read_Entries (Stream, File, EV, H);
		Read_Blocks (Stream, File, BV, H);

		Close (File); -- No longer needed directly

		-- List information about packages, rather than extracting,
		-- if Bypass setting enabled. See Options for more information.
		case Bypass_Settings.Mode is
			when None => -- Extract as usual
				Extract (File_Name, Output_Dir, EV, BV, H,
					Language_ID => Get_Language_ID (File_Name));
			when From_Reference => -- Print entries with a given Reference
				declare
					Entry_ID : Natural := 0;
				begin
					Reference_Bypass :
					for E of EV loop
						if E.Reference = Bypass_Settings.Target_Reference then
							Put_Line (Hex_String (H.Package_ID)
								& "-"
								& Hex_String (Unsigned_16 (Entry_ID))
								& ":" & Entry_Type'Image (E));
							New_Line;
						end if;

						Entry_ID := @ + 1;
					end loop Reference_Bypass;
				end;
			when From_Entry => -- Print information about a specific Entry
				if H.Package_ID = Bypass_Settings.Target_Package then
					declare
						Entry_ID : Natural := 0;
					begin
						Entry_Bypass :
						for E of EV loop
							if Unsigned_16 (Entry_ID) = Bypass_Settings.Target_Entry then
								Put_Line (Entry_Type'Image (E));
							end if;

							Entry_ID := @ + 1;
						end loop Entry_Bypass;
					end;
				end if;
		end case;
	end Unpack;
end Unpacker.Worker;
