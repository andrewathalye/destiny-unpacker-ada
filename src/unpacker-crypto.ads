with Unpacker.Worker; use Unpacker.Worker;

with Interfaces; use Interfaces;

package Unpacker.Crypto is
	-- Types
	type GCM_Tag is array (1 .. 16) of Unsigned_8;
	Blank_GCM : constant GCM_Tag := (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

	-- Modify nonce based upon package header contents
	procedure Modify_Nonce (H : Header);
end Unpacker.Crypto;
