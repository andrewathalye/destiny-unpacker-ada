with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;
with Unchecked_Deallocation;

with Unpacker.Crypto; use Unpacker.Crypto;

package Unpacker.Package_File is
	-- Entry Normalised Type
	-- Cannot be read from Stream
	type Entry_Type is record
		Reference : Unsigned_32;
		Entry_Type : Unsigned_8;
		Entry_Subtype : Unsigned_8;
		Starting_Block : Unsigned_32;
		Starting_Block_Offset : Unsigned_32;
		File_Size : Unsigned_32;
	end record;

	-- Block Normalised Type
	-- Cannot be read from Stream (not all versions have GCM)
	type Block is record
		Offset : Unsigned_32;
		Size : Unsigned_32;
		Patch_ID : Unsigned_16;
		Bit_Flag : Unsigned_16;
		GCM : GCM_Tag;
	end record;

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

	-- Array types for Entry and Block
	type Entry_Array is array (Natural range <>) of Entry_Type;
	type Block_Array  is array (Natural range <>) of Block;
	type Block_Array_Access is access Block_Array;

	-- Free Block_Array_Access
	procedure Free is new Unchecked_Deallocation
		(Object => Block_Array, Name => Block_Array_Access);

	-- Subprograms
	procedure Read_Blocks (S : Stream_Access;
		F : File_Type;
		V : out Block_Array;
		H : Header);
	procedure Read_Entries (S : Stream_Access;
		F : File_Type;
		V : out Entry_Array;
		H : Header);
	function Read_Header (S : Stream_Access) return Header;

end Unpacker.Package_File;
