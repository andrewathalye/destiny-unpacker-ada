package body Unpacker.Package_File.Types is
	-- To_Type functions: return Enum based upon raw entry data
	function To_Type (R : Unsigned_32) return Entry_Reference_Type is
	(case Mode is
		when d1be =>
			(case R is
				--when 16#00000000# => STRING_BNK,
				--when 16#00000000# => STRING_REF,
				when 16#FFFFFFFF# => JUNK,
				when others => UNK),
		when d1 =>
			(case R is
				when 16#808008BE# => STRING_BNK,
				when 16#8080035A# => STRING_REF,
				--when 16#00000000# => return STRING_REF_IDX;
				--when 16#00000000# => return FONT_REF;
				--when 16#00000000# => return LOAD_ZONE;
				--when 16#00000000# => return MAIN_MODEL;
				--when 16#00000000# => return SUBFILES;
				--when 16#00000000# => return DYN_HEADER;
				--when 16#00000000# => return ANIMATION;
				--when 16#00000000# => return TERRAIN;
				--when 16#00000000# => return MATERIAL;
				--when 16#00000000# => return AUDIO_REF;
				when 16#FFFFFFFF# => JUNK,
				when others => UNK),
		when prebl =>
			(case R is
				when 16#80809A8A# => STRING_BNK,
				when 16#80809A88# => STRING_REF,
				--when 16#00000000# => return STRING_REF_IDX;
				--when 16#00000000# => return FONT_REF;
				--when 16#00000000# => return LOAD_ZONE;
				--when 16#00000000# => return MAIN_MODEL;
				--when 16#00000000# => return SUBFILES;
				--when 16#00000000# => return DYN_HEADER;
				--when 16#00000000# => return ANIMATION;
				--when 16#00000000# => return TERRAIN;
				--when 16#00000000# => return MATERIAL;
				when 16#80808D54# => AUDIO_REF, -- TODO: Test
				when 16#FFFFFFFF# => JUNK,
				when others => UNK),
		when postbl =>
			(case R is
				when 16#808099F1# => STRING_BNK,
				when 16#808099EF# => STRING_REF,
				when 16#80805A09# => STRING_REF_IDX,
				when 16#80803C12# => FONT_REF,
				when 16#808093AD# => LOAD_ZONE,
				when 16#80806D44# => MAIN_MODEL,
				when 16#80806D30# => SUBFILES,
				when 16#80809AD8# => DYN_HEADER,
				when 16#80808BE0# => ANIMATION,
				when 16#80806C81# => TERRAIN,
				when 16#80806DAA# => MATERIAL,
				when 16#808097B8# => AUDIO_REF,
				--when 16#FFFFFFFF# => JUNK,
				when others => UNK));

	function To_Type (T : Unsigned_8) return Entry_Type_Type is
	(case Mode is
		when d1be =>
			(case T is
				when 0 => D1BE_BNK,
				when 8 => D1BE_WEM,
				when others => UNK),
		when d1 =>
			(case T is
				--when 16 => RAW_DATA,
				--when 24 => FONT_FILE,
				when 0 | 8 => THIRD_PARTY,
				--when 27 => VIDEO,
				--when 32 => TEXTURE_MODEL_HEADER,
				--when 40 => MODEL_DATA,
				--when 48 => TEXTURE_UI_DATA,
				--when 33 => DIRECTX_BYTECODE_HEADER,
				--when 41 => DIRECTX_BYTECODE,
				when others => UNK),
		when prebl | postbl => -- PreBL included tentatively
			(case T is
				when 8 | 16 => RAW_DATA,
				when 24 => FONT_FILE,
				when 26 => THIRD_PARTY,
				when 27 => VIDEO,
				when 32 => TEXTURE_MODEL_HEADER,
				when 40 => MODEL_DATA,
				when 48 => TEXTURE_UI_DATA,
				when 33 => DIRECTX_BYTECODE_HEADER,
				when 41 => DIRECTX_BYTECODE,
				when others => UNK));

	function To_Type (ST : Unsigned_8) return Entry_Subtype_Type is
	(case Mode is
		when d1be =>
			(case ST is
				when 28 => D1BE_BNK_WEM,
				when others => UNK),
		when d1 =>
			(case ST is
				when 19 | 20 => BNK_IDX_BUF,
				when 21 => WEM,
				--when 7 => HAVOK,
				--when 0 => VIDEO_UNK,
				--when 0 => USM_TEXREF_DDS,
				--when 3 => VERT_BUF,
				when others => UNK),

		when prebl =>
			(case ST is
				when 5 => BNK_IDX_BUF,
				when 6 => WEM,
				--when 7 => HAVOK, -- TODO Check
				--when 0 => VIDEO_UNK,
				when 0 => USM_TEXREF_DDS,
				--when 3 => VERT_BUF, -- TODO Check
				when others => UNK),
		when postbl =>
			(case ST is
				when 6 => BNK_IDX_BUF,
				when 7 => WEM,
				when 8 => HAVOK,
				when 0 => VIDEO_UNK,
				when 1 => USM_TEXREF_DDS,
				when 4 => VERT_BUF,
				when others => UNK));

end Unpacker.Package_File.Types;
