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

	-- Enumeration for known Reference Types
	type Entry_Reference_Type is (UNK, STRING_BNK, STRING_REF, STRING_REF_IDX, FONT_REF, LOAD_ZONE, MAIN_MODEL, SUBFILES, DYN_HEADER, ANIMATION, TERRAIN, MATERIAL, AUDIO_REF, JUNK);

	-- Enumeration for known Entry Types
	type Entry_Type_Type is (UNK, RAW_DATA, FONT_FILE, THIRD_PARTY, VIDEO, TEXTURE_MODEL_HEADER, MODEL_DATA, TEXTURE_UI_DATA, DIRECTX_BYTECODE_HEADER, DIRECTX_BYTECODE, D1BE_BNK, D1BE_WEM);

	-- Enumeration for known Entry Subtypes
	type Entry_Subtype_Type is (UNK, BNK_IDX_BUF, WEM, HAVOK, VIDEO_UNK, USM_TEXREF_DDS,  VERT_BUF, D1BE_BNK_WEM);

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

	-- Array types for Entry and Block
	type Entry_Array is array (Natural range <>) of Entry_Type;
	type Block_Array  is array (Natural range <>) of Block;
	type Block_Array_Access is access Block_Array;

	-- Free Block_Array_Access
	procedure Free is new Unchecked_Deallocation (Object => Block_Array, Name => Block_Array_Access);

	-- Subprograms
	procedure Read_Blocks (S : Stream_Access; F : File_Type; V : out Block_Array; H : Header);
	procedure Read_Entries (S : Stream_Access; F : File_Type; V : out Entry_Array; H : Header);
	function Read_Header (S : Stream_Access) return Header;

	-- Convert raw value into member of corresponding (version-agnostic) enumerated type
	function To_Type (R : Unsigned_32) return Entry_Reference_Type;
	function To_Type (T : Unsigned_8) return Entry_Type_Type;
	function To_Type (ST : Unsigned_8) return Entry_Subtype_Type;
end Unpacker.Package_File;
