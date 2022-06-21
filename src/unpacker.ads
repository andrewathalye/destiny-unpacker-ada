package Unpacker is
	-- Mode
	type Mode_Type is (d1be, d1, prebl, postbl);
	Mode : Mode_Type := postbl;
end Unpacker;
