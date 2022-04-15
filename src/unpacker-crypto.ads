with Unpacker.Worker; use Unpacker.Worker;

package Unpacker.Crypto is
	-- Modify nonce based upon package header contents
	procedure Modify_Nonce (H : Header);
end Unpacker.Crypto;
