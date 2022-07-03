package body Unpacker.Util is
	-- Hex Digit Array
	Hex_Digits : constant array (0 .. 15) of Character :=
		('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
		'a', 'b', 'c', 'd', 'e', 'f');

	-- Get Language ID (or "") from file name
	-- File name format is generally PLA_NAME_PKID[_XX]_P.pkg,
	-- where PLA is platform, NAME is a package name, PKID is
	-- the Package ID, XX is the language, and P is the Patch ID
	function Get_Language_ID (File_Name : String) return String is
		type Language_Type is (En, Fr, De, It, Pn, Pt, Sp, Ru, Po, Cs, Ct, Mx, Ko);
			-- Pn here means Jpn
	begin
		-- Not a language file if this is not the case
		-- If both characters aren't '_', then it is neither a two-letter
		-- nor three-letter language name.
		if File_Name (File_Name'Last - 8) /= '_'
		and File_Name (File_Name'Last - 9) /= '_'
		then
			return "";
		end if;

		-- Extract and return language value
		case Language_Type'Value
			(File_Name (File_Name'Last - 7 .. File_Name'Last - 6))
		is
			when Pn => -- Should really be jpn
				return "jpn";
			when others =>
				return File_Name (File_Name'Last - 7 .. File_Name'Last - 6);
		end case;
	end Get_Language_ID;

	-- Print Unsigned_16 as big endian hex string
	function Hex_String (Num : Unsigned_16) return String is
	begin
		return	[Hex_Digits (Natural (Shift_Right (Num and 16#f000#, 12))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f00#, 8))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f0#, 4))),
			Hex_Digits (Natural (Num and 16#f#))];
	end Hex_String;

	-- Return Unsigned_16 from big endian hex string
	function From_Hex (Hex : String) return Unsigned_16 is
		(Unsigned_16'Value ("16#" & Hex & "#"));

	-- Print Unsigned_32 as little endian hex string
	-- For compatibility with Ginsor Audio Tool names
	function Hex_String_LE (Num : Unsigned_32) return String is
	begin
		return [Hex_Digits (Natural (Shift_Right (Num and 16#f0#, 4))),
			Hex_Digits (Natural (Num and 16#f#)),
			Hex_Digits (Natural (Shift_Right (Num and 16#f000#, 12))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f00#, 8))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f00000#, 20))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f0000#, 16))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f0000000#, 28))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f000000#, 24)))];
	end Hex_String_LE;

	-- Returns Decimal String for Unsigned_32 without any padding
	function Decimal_String (U : Unsigned_32) return String is
		(Unsigned_32'Image (U) (2 .. Unsigned_32'Image (U)'Last));

	-- Returns a file path given an original file path and a new Patch ID
	function Determine_Patch_Name (File_Name : String;
		Patch_ID : Unsigned_16) return String
	is
		(File_Name (File_Name'First .. File_Name'Last - 5)
		& Unsigned_16'Image (Patch_ID) (2
			.. Unsigned_16'Image (Patch_ID)'Last) & ".pkg");

	-- Optimised function to read data array as one block
	procedure Read_Data_Array
		(Stream : not null access Root_Stream_Type'Class;
		Item : out Data_Array)
	is
		Item_Size : Stream_Element_Offset := Item'Size / Stream_Element'Size;

		I_SEA : Stream_Element_Array (1 .. Item_Size)
		with
			Import => True;
		for I_SEA'Address use Item'Address;
	begin
		Read (Stream.all, I_SEA, Item_Size);
	end Read_Data_Array;

	-- Optimised function to write data array as one block
	procedure Write_Data_Array
		(Stream : not null access Root_Stream_Type'Class;
		Item : in Data_Array)
	is
		Item_Size : constant Stream_Element_Offset :=
			Item'Size / Stream_Element'Size;

		O_SEA : Stream_Element_Array (1 .. Item_Size)
		with
			Import => True;

		for O_SEA'Address use Item'Address;
	begin
		Write (Stream.all, O_SEA);
	end Write_Data_Array;

end Unpacker.Util;
