package Unpacker.Package_File.Types is
	-- Enumeration for known Reference Types
	type Entry_Reference_Type is (UNK, STRING_BNK, STRING_REF, STRING_REF_IDX, FONT_REF, LOAD_ZONE, MAIN_MODEL, SUBFILES, DYN_HEADER, ANIMATION, TERRAIN, MATERIAL, AUDIO_REF, JUNK);

	-- Enumeration for known Entry Types
	type Entry_Type_Type is (UNK, RAW_DATA, FONT_FILE, THIRD_PARTY, VIDEO, TEXTURE_MODEL_HEADER, MODEL_DATA, TEXTURE_UI_DATA, DIRECTX_BYTECODE_HEADER, DIRECTX_BYTECODE, D1BE_BNK, D1BE_WEM);

	-- Enumeration for known Entry Subtypes
	type Entry_Subtype_Type is (UNK, BNK_IDX_BUF, WEM, HAVOK, VIDEO_UNK, USM_TEXREF_DDS,  VERT_BUF, D1BE_BNK_WEM);

	-- Subprograms
	-- Convert raw value into member of corresponding (version-agnostic) enumerated type
	function To_Type (R : Unsigned_32) return Entry_Reference_Type;
	function To_Type (T : Unsigned_8) return Entry_Type_Type;
	function To_Type (ST : Unsigned_8) return Entry_Subtype_Type;

end Unpacker.Package_File.Types;
