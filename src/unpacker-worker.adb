with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams; use Ada.Streams.Stream_IO;

with linoodle; use linoodle;

with Unpacker.Crypto; use Unpacker.Crypto;

package body Unpacker.Worker is
	-- Local Types
	
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
			Discard_4 : Discard_Array (16#22# .. 16#B4#);
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
			Discard_2 : Discard_Array (16#12# .. 16#2F#);
			Patch_ID : Unsigned_16; -- HEX 30 to 31
			Discard_3 : Discard_Array (16#32# .. 16#43#);
			Entry_Table_Offset : Unsigned_32; -- HEX 44 to 47
			Discard_4 : Discard_Array (16#48# .. 16#5F#);
			Entry_Table_Size : Unsigned_32; -- HEX 60 to 63
			Discard_5 : Discard_Array (16#64# .. 16#67#);
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
		else -- postbl TODO Find out if there is a build id field
			H := (R.H_PO.Package_ID, Unsigned_32 (0), R.H_PO.Patch_ID, R.H_PO.Entry_Table_Size, R.H_PO.Entry_Table_Offset, R.H_PO.Block_Table_Size, R.H_PO.Block_Table_Offset);	
		end if;

		return H;
	end Read_Header;

	-- Primary unpacker function
	procedure Unpack (Stream : in Stream_Access; File : Stream_IO.File_Type; Output_Dir : in String) is
		H : Header := Read_Header (Stream);
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
		

		Put_Line ("[Error] Unimplemented");
	end Unpack;
end Unpacker.Worker;
