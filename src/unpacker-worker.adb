with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;
with Ada.Containers.Vectors;

with linoodle; use linoodle;

with Unpacker.Crypto; use Unpacker.Crypto;

package body Unpacker.Worker is
	-- Local Types
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
	-- Cannot be read from Stream
	type Block is record
		Offset : Unsigned_32;
		Size : Unsigned_32;
		Patch_ID : Unsigned_16;
		Bit_Flag : Unsigned_16;
		GCM : GCM_Tag;
	end record;

	-- Vector types for unknown-size collections
	package Entry_Vectors is new Ada.Containers.Vectors ( Index_Type => Natural, Element_Type => Entry_Type);	
	package Block_Vectors is new Ada.Containers.Vectors ( Index_Type => Natural, Element_Type => Block);

	-- Modular I/O types
	package Unsigned_16_IO is new Modular_IO (Num => Unsigned_16);
	package Unsigned_32_IO is new Modular_IO (Num => Unsigned_32);

	-- Print Hex String for Unsigned_16
	function Hex_String (Num : Unsigned_16) return String is
		S : String (1 .. 8); -- 16#XXXX#
		First : Natural := 0;
		Last : Natural := 0;
		O : String (1 .. 4) := "0000"; -- XXXX
		Pad : Natural := 0;
	begin
		Unsigned_16_IO.Put(S, Num, 16);

		-- First is first X, last last X in XXXX, XXX, XX, or X
		for I in S'Range loop
			if S (I) = '#' then
				if First = 0 then
					First := I + 1;
				else
					Last := I - 1;
				end if;
			end if;
		end loop;

		-- Fill in only used digits to ensure consistent length
		Pad := 3 - (Last - First);
		for I in First .. Last loop
			O (Pad + I - First + O'First) := S (I);
		end loop;

		return O;
	end Hex_String;

	-- Extract TODO implement
	procedure Extract (S : Stream_Access; F : Stream_IO.File_Type; EV : in Entry_Vectors.Vector; BV : in Block_Vectors.Vector; H : in Header) is
		-- Constants necessary for extraction
		WEM_TYPE : constant Unsigned_8 := (case Mode is
			when prebl | postbl => 26,
			when d1 => 8);
		WEM_SUBTYPE : constant Unsigned_8 := (case Mode is
			when prebl => 6,
			when postbl => 7,
			when d1 => 21);
		BNK_TYPE : constant Unsigned_8 := (case Mode is
			when prebl | postbl => 26,
			when d1 => 0);
		BNK_SUBTYPE : constant Unsigned_8 := (case Mode is
			when prebl => 5,
			when postbl => 6,
			when d1 => 19);
		BNK_SUBTYPE_EXTRA : constant Unsigned_8 := 20; -- Only used for Destiny 1

		-- Variables
		C : Natural := 0;
	begin
		for E of EV loop
			if E.Entry_Type = WEM_TYPE and E.Entry_Subtype = WEM_SUBTYPE then	
				Put ("WEM: ");
				Unsigned_32_IO.Put (Standard_Output, E.Reference, 0);
				Put_Line (".wem");
			elsif E.Entry_Type = BNK_TYPE and (E.Entry_Subtype = BNK_SUBTYPE or (Mode = d1 and E.Entry_Subtype = BNK_SUBTYPE_EXTRA)) then
				Put_Line ("BNK: " & Hex_String (H.Package_ID) & "-" & Hex_String (Unsigned_16 (C)) & ".bnk");
			end if;
			C := C + 1;
		end loop;
	end Extract;

	-- Read Blocks TODO Needs checking
	procedure Read_Blocks (S : Stream_Access; F : Stream_IO.File_Type; V : out Block_Vectors.Vector; H : Header) is
		type Discard_Array is array (Natural range <>) of Unsigned_8;

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
		Set_Index (F, Stream_IO.Positive_Count (I) + 1); -- TODO: Work out why I + 1 needed
		while I < BOUND loop	
			Raw_Block'Read (S, R);
			Block_Vectors.Append (V, (R.Offset, R.Size, R.Patch_ID, R.Bit_Flag, (case Mode is
				when prebl => R.GCM_PR,
				when postbl => R.GCM_PO,
				when d1 => Blank_GCM)));
			I := I + SIZE;
		end loop;
	end Read_Blocks;

	-- Read Entries
	procedure Read_Entries (S : Stream_Access; F : Stream_IO.File_Type; V : out Entry_Vectors.Vector; H : Header) is
		type Raw_Entry is record
			A : Unsigned_32;
			B : Unsigned_32;
			C : Unsigned_32;
			D : Unsigned_32;
		end record;

		R : Raw_Entry;
		E : Entry_Type;
		I : Unsigned_32 := H.Entry_Table_Offset;
	begin -- TODO: Compare shifting with C++ to ensure translation success
		-- TODO Debug Put_Line ("[Debug] Start reading entries from " & Unsigned_32'Image (I)); -- TODO Debug
		Set_Index (F, Stream_IO.Positive_Count (I + 1)); -- TODO: Work out why it was necessary to set index to I + 1

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

	-- Primary unpacker function
	procedure Unpack (Stream : in Stream_Access; File : Stream_IO.File_Type; Output_Dir : in String) is
		H : Header := Read_Header (Stream);
		E : Entry_Vectors.Vector;
		B : Block_Vectors.Vector;
	begin
		-- TODO Debug
		Put_Line ("[Debug] Header Dump: Package ID" & Unsigned_16'Image (H.Package_ID)
			& " Build ID " & Unsigned_32'Image (H.Build_ID)
			& " Patch ID " & Unsigned_16'Image (H.Patch_ID)
			& " Entry Table Size " & Unsigned_32'Image (H.Entry_Table_Size)
			& " Entry Table Offset " & Unsigned_32'Image (H.Entry_Table_Offset)
			& " Block Table Size " & Unsigned_32'Image (H.Block_Table_Size)
			& " Block Table Offset " & Unsigned_32'Image (H.Block_Table_Offset));
		-- TODO Debug

		Modify_Nonce (H);
		Read_Entries (Stream, File, E, H);
		Read_Blocks (Stream, File, B, H);
		Extract (Stream, File, E, B, H);
	end Unpack;
end Unpacker.Worker;
