with linoodle; use linoodle;
with oodle; use oodle;

with Interfaces.C; use Interfaces; use Interfaces.C;
with System;

package Config is
	function Config_Old_OodleLZ_Decompress (src : System.Address;
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
	return size_t renames LinoodleLZ_Decompress;

	function Config_New_OodleLZ_Decompress (src : System.Address;
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
	return size_t renames OodleLZ_Decompress;
end Config;
