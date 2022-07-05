with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Interfaces; use Interfaces;

package Unpacker.Worker is
	-- Bypass Mode Configuration
	type Bypass_Mode is (None, From_Entry, From_Reference);

	-- Must have a constant size so that Mode can be changed dynamically
	type Bypass_Type (Mode : Bypass_Mode := None) is record
		Target_Reference : Unsigned_32 := 0;
		Target_Package : Unsigned_16 := 0;
		Target_Entry : Unsigned_16 := 0;
	end record;

	Bypass_Settings : Bypass_Type;

	-- Subprograms (see body for documentation)
	procedure Create_Extract_Tasks (Count : Positive);
	procedure Unpack (Stream : Stream_Access;
		File : in out File_Type;
		File_Name : String;
		Output_Dir : String);
end Unpacker.Worker;
