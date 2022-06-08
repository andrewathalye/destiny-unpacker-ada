with Interfaces; use Interfaces;

limited with Unpacker.Package_File;
with Unpacker.Util; use Unpacker.Util;

package Unpacker.Crypto is
	-- Types
	type GCM_Tag is array (1 .. 16) of Unsigned_8;
	type Encryption_Type is (None, Key_A, Key_B);

	Blank_GCM : constant GCM_Tag :=
		(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

	-- Modify nonce based upon package header contents
	procedure Modify_Nonce (H : in Package_File.Header);

	-- Decrypt block of data and store in separate buffer
	procedure Decrypt_Block (B : in Package_File.Block;
		B_B : in Data_Array;
		D_B : out Data_Array);
end Unpacker.Crypto;
