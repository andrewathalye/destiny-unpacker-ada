with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package Unpacker.Worker is
	procedure Unpack (Stream : Stream_Access; File : File_Type; Output_Dir : String);
end Unpacker.Worker;
