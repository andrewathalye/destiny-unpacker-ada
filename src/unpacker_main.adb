with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Directories; use Ada.Directories;
with Ada.Exceptions; use Ada.Exceptions;

with GNAT.Command_Line; use GNAT.Command_Line;

with Unpacker.Worker; use Unpacker.Worker;
with Unpacker.Util; use Unpacker.Util;
with Unpacker.Package_File; use Unpacker.Package_File;
with Unpacker.Package_File.Types; use Unpacker.Package_File.Types;
with Unpacker; use Unpacker;

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

	-- Types
	type String_Access is access constant String;

	-- Variables
	SE : Search_Type;
	SA : Stream_Access;
	D : Directory_Entry_Type;
	F : Stream_IO.File_Type;

	Package_Dir : String_Access := null;
	Output_Dir : String_Access := null;
	Avoid_Languages : Boolean := False;

	Worker_Threads : Positive := 1;

	-- Exception
	Invalid_Arguments : exception;
begin -- Unpacker_Main
	Put_Line ("Destiny Linux Unpacker v2.4");

	Options : loop
		case Getopt ("v: x: t: b: h l") is
			when 'v' => -- Version
				begin
					Unpacker.Mode := Mode_Type'Value (Parameter);
				exception
					when Constraint_Error => raise Invalid_Arguments;
				end;
			when 't' => -- Threads
				begin
					Worker_Threads := Positive'Value (Parameter);
				exception
					when Constraint_Error => raise Invalid_Arguments;
				end;
			when 'x' => -- Optional Types
				if not Optional_Types_Exclusive then
					Optional_Types := [others => False];
					Optional_Types_Exclusive := True;
				end if;

				-- Add referenced type
				begin
					Optional_Types (Optional_Type'Value (Parameter)) := True;
				exception
					when Constraint_Error => raise Invalid_Arguments;
				end;
			when 'b' => -- Bypass Normal Extract and search for specific entry
				-- Raise an error if already bypassing
				if Unpacker.Worker.Bypass_Extract then
					raise Invalid_Arguments;
				else
					Unpacker.Worker.Bypass_Extract := True;
				end if;

				-- Correct format is XXXX-XXXX
				begin
					Unpacker.Worker.Target_Package := From_Hex (Parameter (1 .. 4));
					Unpacker.Worker.Target_Entry := From_Hex (Parameter (6 .. 9));
				exception
					when Constraint_Error => raise Invalid_Arguments;
				end;
			when 'l' => -- Do not unpack language files
				Avoid_Languages := True;

			when 'h' => -- Little Endian hex names for WEM files
				Use_Hex_Reference_LE := True;

			when ASCII.NUL => -- Remaining non-arguments
				Package_Dir := new String'(Get_Argument);
				Output_Dir := new String'(Get_Argument);
				exit Options;
			when others => raise Invalid_Arguments;
		end case;
	end loop Options;

	-- If invalid package or output directory
	if Package_Dir.all = "" or Output_Dir.all = "" then
		raise Invalid_Arguments;
	end if;

	-- Create output directory and ensure input directory exists
	if not Exists (Package_Dir.all) then
		Put_Line (Standard_Error,
			"[Error] Could not open packages directory");
		Set_Exit_Status (Failure);
		return;
	end if;

	-- Create Output Dir if necessary
	if not Exists (Output_Dir.all) then
		Create_Directory (Output_Dir.all);
	end if;

	-- Create extract tasks
	Create_Extract_Tasks (Worker_Threads);

	-- Iterate over pkg files
	Start_Search (SE, Package_Dir.all, "*.pkg");
	Process_Entries :
		while More_Entries (SE) loop
			Get_Next_Entry (SE, D);
			if Is_Latest_Patch_ID (Full_Name (D))
			and -- Skip language files if directed to
				not (Avoid_Languages and Get_Language_ID (Full_Name (D))'Length /= 0)
			then
				Put_Line ("[Info] Unpacking "
					& Simple_Name (D));
				Open (F, In_File, Full_Name (D));
				SA := Stream (F);
				Unpack (Stream => SA,
					File => F,
					File_Name => Full_Name (D),
					Output_Dir => Output_Dir.all);
--					exit; -- TODO Debug
			end if;
		end loop Process_Entries;
	End_Search (SE);
exception
	when Invalid_Arguments | Invalid_Switch =>
		Put_Line ("Usage: "
			& Command_Name
			& " [OPTION] PACKAGE_DIR OUTPUT_DIR");
		Put_Line ("Options:");
		Put_Line ("-x: set the types of files to extract."
			& " One file type may be specified per -x"
			& " option provided. Please see the README"
			& " for a list of all file types.");
		Put_Line ("-t: set the number of worker threads."
			& " The default is one, but significant speedups are"
			& " possible using higher numbers.");
		Put_Line ("-v: set the version of the game files to extract."
			& " See the README for a list and the default.");
		Put_Line ("-b: bypass the normal extract procedure and instead"
			& " search for PACKAGE_ID-ENTRY_ID (both in hex)"
			& " and print its reference.");
		Put_Line ("-h: name by-reference files using little endian hex."
			& " This was the default in Ginsor's Audio Tool, and is"
			& " still used by some project files.");
		Put_Line ("-l: do not extract language-specific entries.");
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
