with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with System;

package linoodle is
	function OodleLZ_Decompress (src : System.Address; src_len : size_t; dest : System.Address; dest_len : size_t; u1 : Integer_64; u2 : Integer_64; u3 : Integer_64; u4 : Integer_64; u5 : Integer_64; u6 : Integer_64; u7 : Integer_64; u8 : Integer_64; u9 : Integer_64; u10 : Integer_64) return size_t
	with
		Import => True,
		External_Name => "OodleLZ_Decompress",
		Convention => C;
end linoodle;
