with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with System;

package openssl is
	type EVP_CIPHER_CTX is new System.Address;
	type EVP_CIPHER is new System.Address;

	EVP_CTRL_AEAD_SET_TAG : constant int := 16#11#; -- From openssl/evp.h

	function EVP_MD_CTX_new return EVP_CIPHER_CTX
	with
		Import => True,
		External_Name => "EVP_MD_CTX_new",
		Convention => C;

	function EVP_aes_128_gcm return EVP_CIPHER
	with
		Import => True,
		External_Name => "EVP_aes_128_gcm",
		Convention => C;

	function EVP_DecryptInit (Context : EVP_CIPHER_CTX; Cipher : EVP_CIPHER; Key : System.Address; IV : System.Address) return int
	with
		Import => True,
		External_Name => "EVP_DecryptInit",
		Convention => C;

	procedure EVP_CIPHER_CTX_ctrl (Context : EVP_CIPHER_CTX; Control_Type : int; Value : int; Data : System.Address)
	with
		Import => True,
		External_Name => "EVP_CIPHER_CTX_ctrl",
		Convention => C;

	function EVP_DecryptUpdate (Context : EVP_CIPHER_CTX; Out_B : System.Address; Out_Len : System.Address; In_B : System.Address; In_Len : int) return int
	with
		Import => True,
		External_Name => "EVP_DecryptUpdate",
		Convention => C;

	procedure EVP_CIPHER_CTX_free (Context : in out EVP_CIPHER_CTX)
	with
		Import => True,
		External_name => "EVP_CIPHER_CTX_free",
		Convention => C;

end openssl;
