package Unpacker is
	type Mode_Type is (d1be, d1, prebl, postbl);
	type Endian_Type is (LE, BE);
	Mode : Mode_Type;

	procedure Unpacker_Main;
end Unpacker;
