with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package Unpacker.Worker is
	procedure Create_Extract_Tasks (Count : Positive);

	procedure Unpack (Stream : Stream_Access;
		File : in out File_Type;
		File_Name : String;
		Output_Dir : String);
end Unpacker.Worker;
