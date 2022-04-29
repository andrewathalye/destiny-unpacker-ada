with Unchecked_Deallocation;
with Interfaces; use Interfaces;

package Unpacker.Util is
	-- Common Buffer Type
	type Data_Array is array (Natural range <>) of Unsigned_8;
	type Data_Array_Access is access Data_Array;

	-- Constant String Access
	type String_Access is access String;

	-- Unchecked Deallocation for Access Types
	procedure Free is new Unchecked_Deallocation
		(Object => Data_Array, Name => Data_Array_Access);
	procedure Free is new Unchecked_Deallocation
		(Object => String, Name => String_Access);

	-- Subprograms
	function Get_Language_ID (File_Name : String) return String;
	function Hex_String (Num : Unsigned_16) return String;
	function Decimal_String (U : Unsigned_32) return String;
	function Determine_Patch_Name (File_Name : String;
		Patch_ID : Unsigned_16) return String;
end Unpacker.Util;
