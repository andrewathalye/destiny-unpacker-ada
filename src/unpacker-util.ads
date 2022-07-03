with Unchecked_Deallocation;
with Interfaces; use Interfaces;
with Ada.Streams; use Ada.Streams;

package Unpacker.Util is
	-- Common Buffer Type
	type Data_Array is array (Natural range <>) of Unsigned_8;
	type Data_Array_Access is access Data_Array;

	-- Optimised Data Array I/O
	procedure Read_Data_Array
		(Stream : not null access Root_Stream_Type'Class;
		Item : out Data_Array);
	procedure Write_Data_Array
		(Stream : not null access Root_Stream_Type'Class;
		Item : in Data_Array);

	-- Tie default Stream attributes
	for Data_Array'Read use Read_Data_Array;
	for Data_Array'Write use Write_Data_Array;

	-- String Access
	type String_Access is access String;

	-- Unchecked Deallocation for Access Types
	procedure Free is new Unchecked_Deallocation
		(Object => Data_Array, Name => Data_Array_Access);
	procedure Free is new Unchecked_Deallocation
		(Object => String, Name => String_Access);

	-- Subprograms
	function Get_Language_ID (File_Name : String) return String;
	function Hex_String (Num : Unsigned_16) return String;
	function Hex_String_LE (Num : Unsigned_32) return String;
	function From_Hex (Hex : String) return Unsigned_16;
	function Decimal_String (U : Unsigned_32) return String;
	function Determine_Patch_Name (File_Name : String;
		Patch_ID : Unsigned_16) return String;
end Unpacker.Util;
