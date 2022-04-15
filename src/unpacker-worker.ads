with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Interfaces; use Interfaces;

with Unpacker; use Unpacker;
package Unpacker.Worker is
		
	-- API-stable header for Destiny packages
	-- Cannot be read directly from Stream
	type Header is record
		Package_ID : Unsigned_16;
		Build_ID : Unsigned_32;
		Patch_ID : Unsigned_16;
		Entry_Table_Size : Unsigned_32;
		Entry_Table_Offset : Unsigned_32;
		Block_Table_Size : Unsigned_32;
		Block_Table_Offset : Unsigned_32;
	end record;

	procedure Unpack (Stream : Stream_Access; File : File_Type; Output_Dir : String);
end Unpacker.Worker;
