with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

function OodleLZ_Decompress (src : System.Address;
	src_len : size_t;
	dest : System.Address;
	dest_len : size_t;
	u1 : Integer_64;
	u2 : Integer_64;
	u3 : Integer_64;
	u4 : Integer_64;
	u5 : Integer_64;
	u6 : Integer_64;
	u7 : Integer_64;
	u8 : Integer_64;
	u9 : Integer_64;
	u10 : Integer_64)
	return size_t
is begin	
	Put_Line (Standard_Error, "[Error] The Linux version of Oodle is currently not"
		& " installed, so decompression is not available. Please see README.md to"
		& " correct this.");
	OS_Exit (-1);
	return 0;
end OodleLZ_Decompress;
