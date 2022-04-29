with Unpacker.Package_File; use Unpacker.Package_File;

package Unpacker.Package_File.Types is

	-- Enum type defining how entry output should be named
	type Name_Type is (By_ID, By_Reference);

	-- Record to help Worker determine whether Entry should be extracted
	type Entry_Info_Type is record
		Subdir : String (1 .. 3) := "unk";
		Ext : String (1 .. 3) := "bin";
		Name : Name_Type := By_ID;
		Should_Extract : Boolean := False;
	end record;

	-- Subprograms
	function Get_Info (E : Entry_Type;
		Language_ID : String)
	return Entry_Info_Type;
end Unpacker.Package_File.Types;
