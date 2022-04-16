limited with Unpacker.Package_File;

with Interfaces; use Interfaces;

package Unpacker.Crypto is
	-- Types
	type GCM_Tag is array (1 .. 16) of Unsigned_8;
	Blank_GCM : constant GCM_Tag := (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

	-- Modify nonce based upon package header contents
	procedure Modify_Nonce (H : in Package_File.Header);

	-- Decrypt block of data and store in separate buffer
	procedure Decrypt_Block (B : in Package_File.Block; B_B : in Package_File.Data_Array; D_B : out Package_file.Data_Array);
end Unpacker.Crypto;
