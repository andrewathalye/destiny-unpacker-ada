Destiny Ada Unpacker
======================

This is a high-performance multithreaded unpacker for Destiny 1 and 2, written in Ada.  

>	Note: To comply with legal requirements, only use this program for personal,
>	educative purposes and to bolster interoperability of Destiny (2) with other
>	programs. Do not publish the contents of any Destiny package files without
>	Bungie's explicit permission.

Setup and Usage
---------------

If you would like to build from source, see that section. Otherwise,
download a release from the Releases section (if available).

Destiny 1 and 2 packages are compressed with a library called Oodle, which is
distributed under an all rights reserved license and thus cannot be included.

**Please note:**  

Oodle libraries are _not_ required if you are only interested in extracting WEM music
files from the game. "Dummy" versions of these libraries are included that do not work,
but will allow program operation.  

If you do need to extract other file types, however, follow these steps:  

	- If you intend to use Destiny 1 or Destiny 2 before Beyond Light,
	acquire oo2core_3_win64.dll from the Destiny 2 Shadowkeep game install
	directory and place it in the same directory as the tool.

	- (On Linux) If you intend to use Destiny 2 after Beyond Light,
	acquire oo2corelinux64.so.9 and replace the dummy file in ext_lib with it.
	You can find this file in the Unreal Engine 4/5 Source Code Repository.
	See the warning in the Reference
	section to understand the legal implications of using these files.  

	- (On Windows) If you intend to use Destiny 2 after Beyond Light,
	acquire oo2core_9_win64.dll and place it in the same directory as the tool. You can
	get this file from the Destiny 2 Witch Queen game install.  

Windows-specific information:  
You should rename the Oodle library you currently need to oo2core_3_win64.dll and place it alongside the unpacker executable, regardless of the actual version.  
If, for example, you want to unpack a Beyond Light package, you would rename oo2core_9_win64.dll to oo2core_3_win64.dll.  
If you instead want to unpack Shadowkeep or Destiny 1, you would rename the original oo2core_3_win64.dll file back to its original name.  

Run the program with no arguments to see the available options and syntax.

If any errors occur during unpacking, please add them to an Issue so I can investigate.  

Building from Source on Windows
-------------------------------

To build from source, you'll need GNAT FSF 12.1.0 or higher (from GCC). GPRBuild is also required.  
GNAT GPL 2021 may also work, but I've begun to use Ada 2022 features so there may be some edge cases.  

As always, please feel free to reach out if you need any help.  

You'll need oo2core_3_win64.dll in the main directory alongside destiny_unpacker.gpr, as well as libcrypto-3-x64.dll in the ext_lib folder
for the build to succeed.  

Run `gprbuild -Pdestinyunpacker -Xmode=static` to build the program. A dynamic build is not recommended for Windows because GNAT
libraries are unlikely to be on your PATH.

Building from Source on Linux
-----------------------------
To build from source, you'll need GNAT FSF 12.1.0 or higher (from GCC). GPRBuild, Ninja, and OpenSSL are also needed. 
GNAT GPL 2021 may also work, but I've begun to use Ada 2022 features so there may be some edge cases.  

Important: Make sure to clone this repository, rather than downloading a source code tarball, since it uses Git submodules.  
Run `ninja` to build necessary library files.  
This will produce a build of linoodle and the "dummy" library files necessary to build the program.  
Follow the instructions above to acquire the real Oodle libraries if you need them.  

Run `gprbuild -Pdestinyunpacker` to build the program in development mode, or `gprbuild -Pdestinyunpacker -Xmode=static` for release mode.  
Development mode produces smaller, faster executables, but they cannot be given to other people or released since they have GNAT dependencies.  

Reference
---------

>	Note: This program is for use for educational purposes only.
>	RAD Game Tools, Epic Game Tools, etc. reserve the right to define the terms under which users may
>	use Oodle and related software, to which they own the copyright.
>	If unsure, you might consider purchasing a license to Oodle 2.9.

>	Even though this program is licensed under the GPL, its linking with Oodle makes the actual license unclear.
>	I optionally make this program available under the terms of the LGPL v3 for anyone whom that would assist.

Currently supported versions of the game are d1be (Destiny 1 Big Endian / PS3), d1 (Destiny 1 Little Endian / PS4),
prebl (Destiny 2 before Beyond Light), and postbl (Destiny 2 after Beyond Light).  

Currently available optional file formats are wem (raw audio), bnk (WWise composition format), usm (cutscene format), vox (voice line references), txt (string files and references), and unk (everything else).  

If you would like to decode string files, please use https://github.com/andrewathalye/destiny-string-tool, as these have a convoluted format.  
If you need to convert WEM files to WAV, VGMStream should work quite well.  
If you need to convert BNK files to a textual representation, please try WWiser or https://github.com/andrewathalye/wwtools-ada/  

Credit
------

Thank you to nblockbuster and MontagueM, without whose work on DestinyUnpackerCPP this
project would never have been possible.

https://www.github.com/nblockbuster/DestinyUnpackerCPP and
https://www.github.com/MontagueM/DestinyUnpackerCPP

Additionally, I give my thanks to McSimp and https://github.com/McSimp/linoodle, without which
I would have been unable to decompress most pre-Beyond Light packages on Linux.
