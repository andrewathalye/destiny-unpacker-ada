with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Directories; use Ada.Directories;

with Unpacker.Worker; use Unpacker.Worker;

package body Unpacker is
	procedure Unpacker_Main is
		SE : Search_Type;
		SA : Stream_Access;
		D : Directory_Entry_Type;
		F : Stream_IO.File_Type;

		Invalid_Arguments : exception;

	begin
		Put_Line ("Destiny Linux Unpacker v0.2");

		-- Check for sufficient arguments
		if Argument_Count /= 3 then
			raise Invalid_Arguments;
		end if;

		-- Don't rename until after enough arguments are found (avoid Constraint_Error)
		declare
			Mode_String : String renames Argument (1);
			Package_Dir : String renames Argument (2);
			Output_Dir : String renames Argument (3);
		begin
			-- Check for valid package type
			Mode := Mode_Type'Value (Mode_String);

			-- Create output directory and ensure input directory exists	
			if not Exists (Package_Dir) then
				Put_Line (Standard_Error, "[Error] Could not open packages directory");	
				Set_Exit_Status (Failure);
				return;
			end if;

			if not Exists (Output_Dir) then -- Output dir
				Create_Directory (Output_Dir);
				if not Exists (Output_Dir & "/wem/") then
					Create_Directory (Output_Dir & "/wem/");
				end if;
				if not Exists (Output_Dir & "/bnk/") then
					Create_Directory (Output_Dir & "/bnk/");
				end if;
			end if;

			-- Iterate over pkg files
			Start_Search (SE, Package_Dir, "*.pkg");
			while More_Entries (SE) loop
				Get_Next_Entry (SE, D);
				Put_Line ("[Debug] " & Simple_Name (D)); -- TODO Debug
				Open (F, In_File, Full_Name (D));
				SA := Stream (F);
				Unpack (Stream => SA, File => F, File_Name => Full_Name (D), Output_Dir => Output_Dir);
				Close (F);
				return; -- TODO Debug with one file at once
			end loop;
			End_Search (SE);
		end;
	exception
		when Invalid_Arguments =>
			Put_Line ("Usage: " & Command_Name & " TYPE PACKAGE_DIR OUTPUT_DIR");
			Put_Line ("Types are one of: d1 = Destiny 1, prebl = Pre-Beyond Light, or postbl = Post-Beyond Light");
			return;	
	end Unpacker_Main;
end Unpacker;
