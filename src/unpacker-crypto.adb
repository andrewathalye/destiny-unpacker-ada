with OpenSSL; use OpenSSL;

with Unpacker; use Unpacker;
package body Unpacker.Crypto is
	-- Local Types
	type Key_Type is array (1 .. 16) of Unsigned_8;
	type Nonce_Type is array (1 .. 12) of Unsigned_8;

	-- AES decryption keys (sourced from C++ project)
	AES_KEY_A : aliased constant Key_Type := (16#D6#, 16#2A#, 16#B2#, 16#C1#, 16#0C#, 16#C0#, 16#1B#, 16#C5#, 16#35#, 16#DB#, 16#7B#, 16#86#, 16#55#, 16#C7#, 16#DC#, 16#3B#);
	AES_KEY_B : aliased constant Key_Type := (16#3A#, 16#4A#, 16#5D#, 16#36#, 16#73#, 16#A6#, 16#60#, 16#58#, 16#7E#, 16#63#, 16#E6#, 16#76#, 16#E4#, 16#08#, 16#92#, 16#B5#);

	-- Nonces (sourced from C++ project)
	NONCE_POSTBL : constant Nonce_Type := (16#84#, 16#EA#, 16#11#, 16#C0#, 16#AC#, 16#AB#, 16#FA#, 16#20#, 16#33#, 16#11#, 16#26#, 16#99#);
	NONCE_PREBL : constant Nonce_Type := (16#84#, 16#DF#, 16#11#, 16#C0#, 16#AC#, 16#AB#, 16#FA#, 16#20#, 16#33#, 16#11#, 16#26#, 16#99#);
	Nonce : aliased Nonce_Type; -- The real nonce to be used, depends on package data

	-- Modify Nonce based upon package contents
	-- TODO: Identify whether removing & 0xFF from rightshifted was acceptable
	procedure Modify_Nonce (H : Header) is
	begin
 		if Mode = prebl then
			Nonce := NONCE_PREBL;
			Nonce (1) := NONCE_PREBL (1) xor Unsigned_8 (Shift_Right (H.Package_ID, 8));
			Nonce (2) := NONCE_PREBL (2) xor 16#26#;
			Nonce (12) := NONCE_PREBL (12) xor Unsigned_8 (H.Package_ID and 16#FF#);
		elsif Mode = postbl then -- No need for d1 nonces, since there is no encryption
			Nonce := NONCE_POSTBL;
			Nonce (1) := NONCE_POSTBL (1) xor Unsigned_8 (Shift_Right (H.Package_ID, 8));
			Nonce (12) := NONCE_POSTBL (12) xor Unsigned_8 (H.Package_ID and 16#FF#);
		end if;
	end Modify_Nonce;
end Unpacker.Crypto;
