with Unpacker; use Unpacker;
--with Ada.Text_IO; use Ada; -- TODO Debug

package body Unpacker.Package_File is
	-- Local Types
	type Discard_Array is array (Natural range <>) of Unsigned_8;

	-- Read Blocks
	procedure Read_Blocks (S : Stream_Access; F : File_Type; V : out Block_Array; H : Header) is
		-- Block Raw Type
		-- May be read from Stream
		type Raw_Block (Mode : Mode_Type) is record
			Offset : Unsigned_32; -- HEX 0 .. 3
			Size : Unsigned_32; -- HEX 4 .. 7
			Patch_ID : Unsigned_16; -- HEX 8 .. 9
			Bit_Flag : Unsigned_16; -- HEX A .. B
			SHA_Hash : Discard_Array (16#C# .. 16#1F#);
			case Mode is
				when prebl | postbl =>
					GCM : GCM_Tag; -- HEX 20 .. 2F
				when d1 => -- D1 does not use encryption
					null;
			end case;
		end record;

		R : Raw_Block (Mode);
		File_Index : Unsigned_32 := H.Block_Table_Offset;
		Array_Index : Natural := V'First;
		SIZE : constant Unsigned_32 := (if Mode = d1 then 16#20# else 16#30#); -- D1 blocks lack GCM tag
		BOUND : constant Unsigned_32 := H.Block_Table_Offset + H.Block_Table_Size * SIZE;
	begin
		Set_Index (F, Positive_Count (File_Index + 1)); -- Index starts with 1
		while File_Index < BOUND loop	
			Raw_Block'Read (S, R);
			V (Array_Index) := (R.Offset, R.Size, R.Patch_ID, R.Bit_Flag, (case Mode is
				when prebl | postbl => R.GCM,
				when d1 => Blank_GCM));

			-- Increment indices
			File_Index := File_Index + SIZE;
			Array_Index := Array_Index + 1;

			-- TODO Debug
--			Text_IO.Put_Line ("File Index" & Unsigned_32'Image (File_Index)
--				& " Offset " & Unsigned_32'Image (R.Offset)
--				& " Size " & Unsigned_32'Image (R.Size)
--				& " Patch_ID " & Unsigned_16'Image (R.Patch_ID)
--				& " Bit Flag " & Unsigned_16'Image (R.Bit_Flag));
			-- TODO Debug
		end loop;
	end Read_Blocks;

	-- Read Entries
	procedure Read_Entries (S : Stream_Access; F : File_Type; V : out Entry_Array; H : Header) is
		type Raw_Entry is record
			A : Unsigned_32;
			B : Unsigned_32;
			C : Unsigned_32;
			D : Unsigned_32;
		end record;

		R : Raw_Entry;
		E : Entry_Type;
		File_Index : Unsigned_32 := H.Entry_Table_Offset;
		Array_Index : Natural := V'First;
	begin
--		Text_IO.Put_Line ("[Debug] Start reading entries from " & Unsigned_32'Image (I)); -- TODO Debug
		Set_Index (F, Positive_Count (File_Index + 1)); -- Index in Ada starts with 1

		while File_Index < H.Entry_Table_Offset + H.Entry_Table_Size * 16 loop
			Raw_Entry'Read (S, R);
			-- TODO Debug
--			Text_IO.Put_Line ("[Debug] A " & Unsigned_32'Image (R.A)
--				& " B " & Unsigned_32'Image (R.B)
--		        	& " C " & Unsigned_32'Image (R.C)
--		        	& " D " & Unsigned_32'Image (R.D));
			-- TODO Debug
			E.Reference := R.A;

			if Mode = d1 then -- TODO: Check if crunching occurs for Entry_Type
				E.Entry_Type := Unsigned_8 (R.B and 16#FF#);
				E.Entry_Subtype := Unsigned_8 (Shift_Right (R.B, 24));
			else
				E.Entry_Type := Unsigned_8 (Shift_Right (R.B, 9) and 16#7F#);
				E.Entry_Subtype := Unsigned_8 (Shift_Right (R.B, 6) and 7);
			end if;

			E.Starting_Block := R.C and 16#3FFF#;
			E.Starting_Block_Offset := Shift_Left ((Shift_Right (R.C, 14) and 16#3FFF#), 4);

			E.File_Size := Shift_Left (R.D and 16#3FFFFFF#, 4) or (Shift_Right (R.C, 28) and 16#F#);

			V (Array_Index) := E; -- Add Entry to array

			-- Increment indices
			File_Index := File_Index + 16;
			Array_Index := Array_Index + 1;

			-- TODO Debug
--			Text_IO.Put_Line ("[Debug] Reference " & Unsigned_32'Image (E.Reference)
--				& " Entry Type " & Unsigned_8'Image (E.Entry_Type)
--				& " Entry Subtype " & Unsigned_8'Image (E.Entry_Subtype)
--				& " Starting Block " & Unsigned_32'Image (E.Starting_Block)
--				& " Starting Block Offset " & Unsigned_32'Image (E.Starting_Block_Offset)
--				& " File Size " & Unsigned_32'Image (E.File_Size));
			-- TODO Debug
		end loop;
	end Read_Entries;

	-- Read Header given Stream
	function Read_Header (S : Stream_Access) return Header is
		-- Local Types
		-- Array of bytes to skip when reading
		type Discard_Array is array (Integer range <>) of Unsigned_8;

		-- PreBL and D1 Header Structure
		type D1_PreBL_Header is record
			Discard_1 : Discard_Array (0 .. 3);
			Package_ID : Unsigned_16; -- HEX 4 to 5
			Discard_2 : Discard_Array (6 .. 16#17# );
			Build_ID : Unsigned_32; -- HEX 18 to 1B
			Discard_3 : Discard_Array  (16#1C# .. 16#1F#);
			Patch_ID : Unsigned_16; -- HEX 20 to 21
			Discard_4 : Discard_Array (16#22# .. 16#B3#);
			Entry_Table_Size : Unsigned_32; -- HEX B4 to B7
			Entry_Table_Offset : Unsigned_32; -- HEX B8 to BB NOT ALWAYS
			Discard_5 : Discard_Array (16#BC# .. 16#CF#);
			Block_Table_Size : Unsigned_32; -- HEX D0 to D3
			Block_Table_Offset : Unsigned_32; -- HEX D4 to D7 NOT ALWAYS
			Discard_6 : Discard_Array (16#D8# .. 16#10F#);
			Alternate_Entry_Table_Offset : Unsigned_32; -- HEX 110 to 113
		end record;

		-- PostBL Header Structure
		type PostBL_Header is record
			Discard_1 : Discard_Array (0 .. 16#0F#);
			Package_ID : Unsigned_16; -- HEX 10 to 11
			Discard_2 : Discard_Array (16#12# .. 16#27#);
			Build_ID : Unsigned_32; -- HEX 28 to 2B
			Discard_3 : Discard_Array (16#2C# .. 16#2F#);
			Patch_ID : Unsigned_16; -- HEX 30 to 31
			Discard_4 : Discard_Array (16#32# .. 16#43#);
			Entry_Table_Offset : Unsigned_32; -- HEX 44 to 47
			Discard_5 : Discard_Array (16#48# .. 16#5F#);
			Entry_Table_Size : Unsigned_32; -- HEX 60 to 63
			Discard_6 : Discard_Array (16#64# .. 16#67#);
			Block_Table_Size : Unsigned_32; -- HEX 68 to 6B
			Block_Table_Offset : Unsigned_32; -- HEX 6C to 6F
		end record;

		-- Suitable for reading directly from Stream_Access
		type Raw_Header (Mode : Mode_Type) is record
			case Mode is
				when d1 | prebl =>
					H_D1PR : D1_PreBL_Header;
				when postbl =>
					H_PO : PostBL_Header;
			end case;
		end record;

		H : Header;
		R : Raw_Header (Mode);
	begin
		Raw_Header'Read (S, R);

		-- Correct internal data for Forsaken to Shadowkeep
		-- Note: this is a designation about individual packages, not the whole folder
		if Mode = prebl and R.H_D1PR.Build_ID > 16#10000# then -- Forsaken and Later
			R.H_D1PR.Entry_Table_Offset := R.H_D1PR.Alternate_Entry_Table_Offset + 96;
			R.H_D1PR.Block_Table_Offset := R.H_D1PR.Entry_Table_Offset + R.H_D1PR.Entry_Table_Size * 16 + 32;
		end if;

		if Mode = prebl or Mode = d1 then
			H := (R.H_D1PR.Package_ID, R.H_D1PR.Build_ID, R.H_D1PR.Patch_ID, R.H_D1PR.Entry_Table_Size, R.H_D1PR.Entry_Table_Offset, R.H_D1PR.Block_Table_Size, R.H_D1PR.Block_Table_Offset);	
		else -- postbl
			H := (R.H_PO.Package_ID, R.H_PO.Build_ID, R.H_PO.Patch_ID, R.H_PO.Entry_Table_Size, R.H_PO.Entry_Table_Offset, R.H_PO.Block_Table_Size, R.H_PO.Block_Table_Offset);	
		end if;

		return H;
	end Read_Header;

	-- To_Type functions: return Enum based upon raw entry data
	function To_Type (R : Unsigned_32) return Entry_Reference_Type is
	(case Mode is
		when d1 =>
			(case R is
				when 16#808008BE# => STRING_BNK,
				--when 16#00000000# => STRING_REF,
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
		when d1 =>
			(case T is
				--when 8 | 16 => RAW_DATA,
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
		when d1 =>
			(case ST is
				when 19 => BNK_IDX_BUF,
				when 20 => BNK_IDX_BUF,
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
end Unpacker.Package_File;
