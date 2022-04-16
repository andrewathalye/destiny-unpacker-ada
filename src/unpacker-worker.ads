with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Interfaces; use Interfaces;

with Unpacker; use Unpacker;
package Unpacker.Worker is
	procedure Unpack (Stream : Stream_Access; File : File_Type; File_Name : String; Output_Dir : String);
end Unpacker.Worker;
