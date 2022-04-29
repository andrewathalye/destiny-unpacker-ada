with Interfaces.C; use Interfaces.C;
with System;

package OpenSSL is
	type EVP_CIPHER_CTX is null record;
	type EVP_CIPHER is null record;

	EVP_CTRL_AEAD_SET_TAG : constant := 16#11#; -- From openssl/evp.h

	function EVP_CIPHER_CTX_new return access EVP_CIPHER_CTX
	with
		Import => True,
		Convention => C,
		External_Name => "EVP_CIPHER_CTX_new";

	function EVP_aes_128_gcm return access constant EVP_CIPHER
	with
		Import => True,
		Convention => C,
		External_Name => "EVP_aes_128_gcm";

	function EVP_DecryptInit (Context : access EVP_CIPHER_CTX;
		Cipher : access constant EVP_CIPHER;
		Key : System.Address;
		IV : System.Address)
	return int
	with
		Import => True,
		Convention => C,
		External_Name => "EVP_DecryptInit";

	procedure EVP_CIPHER_CTX_ctrl (Context : access EVP_CIPHER_CTX;
		Control_Type : int;
		Value : int;
		Data : System.Address)
	with
		Import => True,
		Convention => C,
		External_Name => "EVP_CIPHER_CTX_ctrl";

	function EVP_DecryptUpdate (Context : access EVP_CIPHER_CTX;
		Out_B : System.Address;
		Out_Len : access int;
		In_B : System.Address;
		In_Len : int)
	return int
	with
		Import => True,
		Convention => C,
		External_Name => "EVP_DecryptUpdate";

	procedure EVP_CIPHER_CTX_free (Context : access EVP_CIPHER_CTX)
	with
		Import => True,
		Convention => C,
		External_Name => "EVP_CIPHER_CTX_free";

end OpenSSL;
