package Unpacker is
	type Mode_Type is (d1be, d1, prebl, postbl);
	Mode : Mode_Type := postbl;

	procedure Unpacker_Main;
end Unpacker;
