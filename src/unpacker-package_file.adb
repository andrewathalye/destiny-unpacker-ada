with Unpacker; use Unpacker;
with Ada.Text_IO; use Ada; -- TODO Debug

package body Unpacker.Package_File is
	-- Local Types
	type Discard_Array is array (Natural range <>) of Unsigned_8;

	-- Read Blocks TODO Needs checking
	procedure Read_Blocks (S : Stream_Access; F : File_Type; V : out Block_Vectors.Vector; H : Header) is

		-- Block Raw Type
		-- May be read from Stream
		type Raw_Block (Mode : Mode_Type) is record
			Offset : Unsigned_32; -- HEX 0 .. 3
			Size : Unsigned_32; -- HEX 4 .. 7
			Patch_ID : Unsigned_16; -- HEX 8 .. 9
			Bit_Flag : Unsigned_16; -- HEX A .. B

			case Mode is
				when prebl =>
					Discard_PR : Discard_Array (16#C# .. 16#21# ); -- Check math
					GCM_PR : GCM_Tag;
				when postbl =>
					Discard_PO : Discard_Array (16#C# .. 16#1F# );
					GCM_PO : GCM_Tag;
				when others => -- D1 does not use encryption
					null;
			end case;
		end record;

		R : Raw_Block (Mode);
		I : Unsigned_32 := H.Block_Table_Offset;
		SIZE : constant Unsigned_32 := (if Mode = d1 then 32 else 48); -- D1 blocks lack GCM tag
		BOUND : constant Unsigned_32 := H.Block_Table_Offset + H.Block_Table_Size * SIZE;
	begin
		Set_Index (F, Positive_Count (I) + 1); -- Index starts with 1
		while I < BOUND loop	
			Raw_Block'Read (S, R);
			-- Text_IO.Put_Line ("[Debug] Patch ID is " & Unsigned_16'Image (R.Patch_Id));
			Block_Vectors.Append (V, (R.Offset, R.Size, R.Patch_ID, R.Bit_Flag, (case Mode is
				when prebl => R.GCM_PR,
				when postbl => R.GCM_PO,
				when d1 => Blank_GCM)));
			I := I + SIZE;
		end loop;
	end Read_Blocks;

	-- Read Entries
	procedure Read_Entries (S : Stream_Access; F : File_Type; V : out Entry_Vectors.Vector; H : Header) is
		type Raw_Entry is record
			A : Unsigned_32;
			B : Unsigned_32;
			C : Unsigned_32;
			D : Unsigned_32;
		end record;

		R : Raw_Entry;
		E : Entry_Type;
		I : Unsigned_32 := H.Entry_Table_Offset;
	begin
		-- TODO Debug Put_Line ("[Debug] Start reading entries from " & Unsigned_32'Image (I)); -- TODO Debug
		Set_Index (F, Positive_Count (I + 1)); -- Index in Ada starts with 1

		while I < H.Entry_Table_Offset + H.Entry_Table_Size * 16 loop
			Raw_Entry'Read (S, R);
			-- TODO Debug
--			Put_Line ("[Debug] A " & Unsigned_32'Image (R.A)
--				& " B " & Unsigned_32'Image (R.B)
--				& " C " & Unsigned_32'Image (R.C)
--				& " D " & Unsigned_32'Image (R.D));
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
			Entry_Vectors.Append (V, E); -- Add Entry to linked list

			I := I + 16;

			-- TODO Debug
--			Put_Line ("[Debug] Reference " & Unsigned_32'Image (E.Reference)
--				& " Entry Type " & Unsigned_8'Image (E.Entry_Type)
--				& " Entry Subtype " & Unsigned_8'Image (E.Entry_Subtype)
--				& " Starting Block " & Unsigned_32'Image (E.Starting_Block)
--				& " Starting Block Offset " & Unsigned_32'Image (E.Starting_Block_Offset)
--				& " File Size " & Unsigned_32'Image (E.File_Size));
			-- TODO Debug
		end loop;
	end Read_Entries;

	-- Read Header given Stream
	function Read_Header (S : in Stream_Access) return Header is
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
end Unpacker.Package_File;