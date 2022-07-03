with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Interfaces; use Interfaces;

package Unpacker.Worker is
	-- Bypass Mode Global Variables
	Bypass_Extract : Boolean := False;
	Target_Package : Unsigned_16;
	Target_Entry : Unsigned_16; -- TODO: Update in case of entry table expansion

	-- Subprograms (see body for documentation)
	procedure Create_Extract_Tasks (Count : Positive);
	procedure Unpack (Stream : Stream_Access;
		File : in out File_Type;
		File_Name : String;
		Output_Dir : String);
end Unpacker.Worker;
