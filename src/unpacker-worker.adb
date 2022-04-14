with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;

package body Unpacker.Worker is
	procedure Unpack (Stream : Stream_Access; File : Stream_IO.File_Type; Output_Dir : in String) is
	begin
		Put_Line ("[Error] Unimplemented");
	end Unpack;
end Unpacker.Worker;
