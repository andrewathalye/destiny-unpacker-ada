package body Unpacker.Package_File.Types is
	-- Enumeration for known Reference Types
	type Entry_Reference_Type is
		(Unknown, String_Bank, String_Reference, String_Reference_Index,
		Font_Reference, Load_Zone, Main_Model, Subfiles, Dynamic_Header,
		Animation, Terrain, Material, Audio_Reference, Junk);

	-- Enumeration for known Entry Types
	type Entry_Type_Type is (Unknown, Raw_Data, Font_File, Third_Party, Video,
		Texture_Model_Header, Model_Data, Texture_UI_Data,
		DirectX_Bytecode_Header, DirectX_Bytecode, D1BE_BNK, D1BE_WEM);

	-- Enumeration for known Entry Subtypes
	type Entry_Subtype_Type is (Unknown, BNK_Index_Buffer, WEM, Havok,
		Video_Unknown, USM_Text_Reference_DDS, Vertex_Buffer, D1BE_BNK_WEM);

	-- To_Type functions: return Enum based upon raw entry data
	function To_Type (R : Unsigned_32) return Entry_Reference_Type is
	(case Mode is
		when d1be =>
			(case R is
				when 16#808008BE# => String_Bank, -- TODO Test
				when 16#8080035A# => String_Reference,
				when 16#FFFFFFFF# => Junk,
				when others => Unknown),
		when d1 =>
			(case R is
				when 16#808008BE# => String_Bank,
				when 16#8080035A# => String_Reference,
				-- when 16#00000000# => return STRING_REF_IDX;
				-- when 16#00000000# => return FONT_REF;
				-- when 16#00000000# => return LOAD_ZONE;
				-- when 16#00000000# => return MAIN_MODEL;
				-- when 16#00000000# => return SUBFILES;
				-- when 16#00000000# => return DYN_HEADER;
				-- when 16#00000000# => return ANIMATION;
				-- when 16#00000000# => return TERRAIN;
				-- when 16#00000000# => return MATERIAL;
				-- when 16#00000000# => return AUDIO_REF;
				when 16#FFFFFFFF# => Junk,
				when others => Unknown),
		when prebl =>
			(case R is
				when 16#80809A8A# => String_Bank,
				when 16#80809A88# => String_Reference,
				-- when 16#00000000# => return STRING_REF_IDX;
				-- when 16#00000000# => return FONT_REF;
				-- when 16#00000000# => return LOAD_ZONE;
				-- when 16#00000000# => return MAIN_MODEL;
				-- when 16#00000000# => return SUBFILES;
				-- when 16#00000000# => return DYN_HEADER;
				-- when 16#00000000# => return ANIMATION;
				-- when 16#00000000# => return TERRAIN;
				-- when 16#00000000# => return MATERIAL;
				when 16#80808D54# => Audio_Reference, -- TODO: Test
				when 16#FFFFFFFF# => Junk,
				when others => Unknown),
		when postbl =>
			(case R is
				when 16#808099F1# => String_Bank,
				when 16#808099EF# => String_Reference,
				when 16#80805A09# => String_Reference_Index,
				when 16#80803C12# => Font_Reference,
				when 16#808093AD# => Load_Zone,
				when 16#80806D44# => Main_Model,
				when 16#80806D30# => Subfiles,
				when 16#80809AD8# => Dynamic_Header,
				when 16#80808BE0# => Animation,
				when 16#80806C81# => Terrain,
				when 16#80806DAA# => Material,
				when 16#808097B8# => Audio_Reference,
				-- when 16#FFFFFFFF# => JUNK,
				when others => Unknown));

	function To_Type (T : Unsigned_8) return Entry_Type_Type is
	(case Mode is
		when d1be =>
			(case T is
				when 0 => D1BE_BNK,
				when 8 => D1BE_WEM,
				when others => Unknown),
		when d1 =>
			(case T is
				-- when 16 => RAW_DATA,
				-- when 24 => FONT_FILE,
				when 0 | 8 => Third_Party,
				-- when 27 => VIDEO,
				-- when 32 => TEXTURE_MODEL_HEADER,
				-- when 40 => MODEL_DATA,
				-- when 48 => TEXTURE_UI_DATA,
				-- when 33 => DIRECTX_BYTECODE_HEADER,
				-- when 41 => DIRECTX_BYTECODE,
				when others => Unknown),
		when prebl | postbl => -- PreBL included tentatively
			(case T is
				when 8 | 16 => Raw_Data,
				when 24 => Font_File,
				when 26 => Third_Party,
				when 27 => Video,
				when 32 => Texture_Model_Header,
				when 40 => Model_Data,
				when 48 => Texture_UI_Data,
				when 33 => DirectX_Bytecode_Header,
				when 41 => DirectX_Bytecode,
				when others => Unknown));

	function To_Type (ST : Unsigned_8) return Entry_Subtype_Type is
	(case Mode is
		when d1be =>
			(case ST is
				when 28 => D1BE_BNK_WEM,
				when others => Unknown),
		when d1 =>
			(case ST is
				when 19 | 20 => BNK_Index_Buffer,
				when 21 => WEM,
				-- when 7 => HAVOK,
				-- when 0 => VIDEO_UNK,
				-- when 0 => USM_TEXREF_DDS,
				-- when 3 => VERT_BUF,
				when others => Unknown),

		when prebl =>
			(case ST is
				when 5 => BNK_Index_Buffer,
				when 6 => WEM,
				-- when 7 => HAVOK, -- TODO Check
				-- when 0 => VIDEO_UNK,
				when 0 => USM_Text_Reference_DDS,
				-- when 3 => VERT_BUF, -- TODO Check
				when others => Unknown),
		when postbl =>
			(case ST is
				when 6 => BNK_Index_Buffer,
				when 7 => WEM,
				when 8 => Havok,
				when 0 => Video_Unknown,
				when 1 => USM_Text_Reference_DDS,
				when 4 => Vertex_Buffer,
				when others => Unknown));

	-- Returns Entry_Info record based upon provided Entry and Language ID
	function Get_Info (E : Entry_Type;
		Language_ID : String)
	return Entry_Info_Type is
		EI : Entry_Info_Type;
		ER : constant Entry_Reference_Type := To_Type (E.Reference);
		ET : Entry_Type_Type := To_Type (E.Entry_Type);
		ES : Entry_Subtype_Type := To_Type (E.Entry_Subtype);
	begin
		-- Correct for Destiny 1 BE types
		if ES = D1BE_BNK_WEM then
			case ET is
				when D1BE_WEM =>
					ET := Third_Party;
					ES := WEM;
				when D1BE_BNK =>
					ET := Third_Party;
					ES := BNK_Index_Buffer;
				when others =>
					ET := Unknown;
					ES := Unknown;
			end case;
		end if;

		-- Check if entry is supported for extraction
		case ET is
			when Third_Party =>
				case ES is
					when WEM =>
						case ER is
							when Junk => null;
								-- Some WEM entries in language packages contain no actual audio
							when others =>
								if Language_ID'Length = 0 then -- Language audio can share References
									if Use_Hex_Reference_LE then
										EI.Name := By_Hex_Reference_LE;
									else
										EI.Name := By_Reference;
									end if;
								end if;
								EI.Subdir := "wem";
								EI.Ext := "wem";
								EI.Should_Extract := True and Optional_Types (wem);
						end case;
					when BNK_Index_Buffer =>
						if Language_ID'Length = 0 then
							-- Banks are not currently useful for language-specific audio
							EI.Should_Extract := True and Optional_Types (bnk);
						end if;

						EI.Subdir := "bnk";
						EI.Ext := "bnk";
					when others => null;
				end case;
			when Video =>
				case ES is
					when USM_Text_Reference_DDS =>
						EI.Subdir := "usm";
						EI.Ext := "usm";
						EI.Should_Extract := True and Optional_Types (usm);
					when others => null;
				end case;
			when others =>
				case ER is
					when String_Bank =>
						EI.Subdir := "txt";
						EI.Ext := "str";
						EI.Should_Extract := True and Optional_Types (txt);
					when String_Reference =>
						EI.Subdir := "txt";
						EI.Ext := "ref";
						EI.Should_Extract := True and Optional_Types (txt);
					when Audio_Reference =>
						EI.Subdir := "vox"; -- For Voice
						EI.Ext := "ref";
						EI.Should_Extract := True and Optional_Types (vox);
					when Junk => null;
					when others =>
						EI.Should_Extract := Optional_Types (unk);
						null;
				end case;
		end case;
		return EI;
	end Get_Info;

end Unpacker.Package_File.Types;
