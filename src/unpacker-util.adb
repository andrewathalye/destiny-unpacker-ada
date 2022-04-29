package body Unpacker.Util is
	-- Get Language ID (or "") from file name
	function Get_Language_ID (File_Name : String) return String is
		type Language_Type is (En, Fr, De, It, Pn, Pt, Sp, Ru, Po, Cs, Ct, Mx, Ko);
			-- Pn here means Jpn
	begin
		case Language_Type'Value
			(File_Name (File_Name'Last - 7 .. File_Name'Last - 6))
		is
			when Pn => -- Should really be jpn
				return "jpn";
			when others => -- Invalid values or any other issue will return "";
				return File_Name (File_Name'Last - 7 .. File_Name'Last - 6);
		end case;
	exception
		when Constraint_Error =>
			-- Invalid value, length, etc. - in any case should return ""
			return "";
	end Get_Language_ID;

	-- Print Unsigned_16 as big endian hex string
	function Hex_String (Num : Unsigned_16) return String is
		-- Hex Digit Array
		Hex_Digits : constant array (0 .. 15) of Character :=
			('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
			'a', 'b', 'c', 'd', 'e', 'f');
	begin
		return	(Hex_Digits (Natural (Shift_Right (Num and 16#f000#, 12))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f00#, 8))),
			Hex_Digits (Natural (Shift_Right (Num and 16#f0#, 4))),
			Hex_Digits (Natural (Num and 16#f#)));
	end Hex_String;

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

end Unpacker.Util;
