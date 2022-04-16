with Ada.Containers.Vectors;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Interfaces; use Interfaces;

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
	-- Cannot be read from Stream (sadly)
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

	-- Buffer Universal Data Type
	type Data_Array is array (Natural range <>) of Unsigned_8;

	-- Vector types for unknown-size collections
	package Entry_Vectors is new Ada.Containers.Vectors ( Index_Type => Natural, Element_Type => Entry_Type);	
	package Block_Vectors is new Ada.Containers.Vectors ( Index_Type => Natural, Element_Type => Block);

	-- Procedures
	procedure Read_Blocks (S : Stream_Access; F : File_Type; V : out Block_Vectors.Vector; H : Header);
	procedure Read_Entries (S : Stream_Access; F : File_Type; V : out Entry_Vectors.Vector; H : Header);
	function Read_Header (S : in Stream_Access) return Header;
end Unpacker.Package_File;
