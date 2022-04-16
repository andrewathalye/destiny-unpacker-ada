with Interfaces.C; use Interfaces.C;
with Interfaces; use Interfaces;
with System;

package ooz is
	function Ooz_Decompress (src : System.Address; src_len : int; dest : System.Address; dest_len : int) return int
	with
		Import => True,
		External_Name => "Ooz_Decompress",
		Convention => C;
end ooz;
