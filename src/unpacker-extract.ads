with Unpacker.Package_File; use Unpacker.Package_File;

package Unpacker.Extract is
	-- Exceptions
	Extract_Exception : exception;
		-- Any error which prevents extraction of an entry
	Extract_Task_Exception : exception;
		-- Fatal error which forces task to terminate
	Package_Cache_Exception : exception;
		-- Failure to cache or free a package file

	-- Task Definition for Parallel Extractions
	task type Extract_Task is
		entry Setup (File_Name : String; BV : Block_Array);
		entry Run (Path : String; E : Entry_Type);
		entry Done;
	end Extract_Task;

	-- Subprograms
	-- Load package file into memory
	-- Must be called before trying to unpack packages
	procedure Cache_Package (File_Name : String);

	-- Free package file from memory
	-- Must be called after unpacking
	procedure Free_Package;

	function Is_Package_Cached return Boolean;
end Unpacker.Extract;
