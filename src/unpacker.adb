with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;

with Unpacker.Worker; use Unpacker.Worker;

package body Unpacker is
	procedure Unpacker_Main is
		-- Subprograms
		-- Check if specified package has the highest patch ID
		function Is_Latest_Patch_ID (F : String) return Boolean
		is
			Current_Patch_ID : constant Natural :=
				Natural'Value (F (F'Last - 4 .. F'Last - 4));
			Base : constant String := F (F'First .. F'Last - 5);
		begin
			if Current_Patch_ID = 9 then
				return True;
			end if;

			for I in Current_Patch_ID + 1 .. 9 loop
				if Exists (Base & Natural'Image (I) (2 .. 2) & ".pkg") then
					return False;
				end if;
			end loop;
			return True;
		end Is_Latest_Patch_ID;

		-- Variables
		SE : Search_Type;
		SA : Stream_Access;
		D : Directory_Entry_Type;
		F : Stream_IO.File_Type;

		-- Exception
		Invalid_Arguments : exception;
	begin -- Unpacker_Main
		Put_Line ("Destiny Linux Unpacker v1.5");

		-- Check for sufficient arguments
		if Argument_Count /= 3 then
			raise Invalid_Arguments;
		end if;

		-- Don't rename until after enough arguments are found
		-- (avoid Constraint_Error)
		declare
			Mode_String : String renames Argument (1);
			Package_Dir : String renames Argument (2);
			Output_Dir : String renames Argument (3);
		begin
			-- Check for valid package type
			begin
				Mode := Mode_Type'Value (Mode_String);
			exception
				when Constraint_Error => raise Invalid_Arguments;
			end;

			-- Create output directory and ensure input directory exists
			if not Exists (Package_Dir) then
				Put_Line (Standard_Error,
					"[Error] Could not open packages directory");
				Set_Exit_Status (Failure);
				return;
			end if;

			-- Create Output Dir if necessary
			if not Exists (Output_Dir) then
				Create_Directory (Output_Dir);
			end if;

			-- Iterate over pkg files
			Start_Search (SE, Package_Dir, "*.pkg");
			Process_Entries :
				while More_Entries (SE) loop
					Get_Next_Entry (SE, D);
					if Is_Latest_Patch_ID (Full_Name (D)) then
						Put_Line ("[Info] Unpacking "
							& Simple_Name (D));
						Open (F, In_File, Full_Name (D));
						SA := Stream (F);
						Unpack (Stream => SA,
							File => F,
							File_Name => Full_Name (D),
							Output_Dir => Output_Dir);
	--					exit; -- TODO Debug
					end if;
				end loop Process_Entries;
			End_Search (SE);
		end;
	exception
		when Invalid_Arguments =>
			Put_Line ("Usage: "
				& Command_Name
				& " TYPE PACKAGE_DIR OUTPUT_DIR");
			Put_Line ("Types are one of: d1be = Destiny 1 Big Endian, d1 = Destiny 1, "
				& "prebl = Pre-Beyond Light, or postbl = Post-Beyond Light");
			return;
		when E : Storage_Error =>
			Put_Line (Standard_Error,
				"[Fatal Error] The Mode type supplied was of the incorrect endianness or "
				& "an incorrect memory access occurred: "
				& Exception_Message (E));
			return;
		when E : Constraint_Error =>
			Put_Line (Standard_Error,
				"[Fatal Error] An unhandled bounds error occurred. This is most likely "
				& "the result of an incorrect Mode. Raw Error: "
				& Exception_Message (E));
			return;
	end Unpacker_Main;
end Unpacker;
