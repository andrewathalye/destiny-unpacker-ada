Destiny Ada Unpacker
======================

This is a high-performance multithreaded unpacker for Destiny 1 and 2, written in Ada.  

On Linux, it makes use of Linoodle (https://www.github.com/McSimp/linoodle) in order to
interface with Oodle, the compressor used by Destiny.

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

In order to decompress files, do the following:  
	- If you intend to use Destiny 1 or Destiny 2 before Beyond Light,
	acquire oo2core_3_win64.dll and place it in the same directory as the tool.  
	- (On Linux) If you intend to use Destiny 2 after Beyond Light,
	acquire oo2corelinux64.so.9 and replace the dummy file in ext_lib.  
	- (On Windows) If you intend to use Destiny 2 after Beyond Light,
	acquire oo2core_9_win64.dll and place it in the same directory as the tool.

To acquire oo2core_3_win64.dll or oo2core_9_win64.dll, just look inside the bin directory of the Destiny 2 Shadowkeep depot folder.  

Windows-specific information:
You should rename the Oodle library you currently need to oo2core_3_win64.dll and place it alongside the unpacker executable, regardless of the actual version.

Linux-specific information:
To acquire liboo2corelinux64.so.9, follow the instructions at https://github.com/gildor2/UEViewer/tree/master/libs/oodle. This process is more involved, so please feel free to reach out for help. Please note that only the Windows version is needed if you intend to unpack Destiny 1 / Pre-Beyond Light.  

>	Note: This program is for use for educational purposes only.
>	RAD Game Tools, Epic Game Tools, etc. reserve the right to define the terms under which users may
>	use Oodle and related software, to which they own the copyright.
>	If unsure, you might consider purchasing a license to Oodle 2.9.

Run the program with no arguments to see the available options and syntax.

If any errors occur during unpacking, please add them to an Issue so I can investigate.  

Building from Source on Windows
-------------------------------

To build from source, you'll need GNAT FSF 12.1.0 or higher (from GCC). GPRBuild is also required.  
GNAT GPL 2021 may also work, but I've begun to use Ada 2022 features so there may be some edge cases.  

To build from source on Windows, you will need a libcrypto.dll file and oo2core_X_win64.dll at build time.  
Configure the GNAT project to use these by modifying external_openssl.gpr and external_oodle.gpr.  
Next modify unpacker-extract.adb so that it always uses the same path regardless of compression version.  

As always, please feel free to reach out if you need any help.  

Run `gprbuild -Pdestinyunpacker -Xmode=static` to build the program. A dynamic build is not recommended for Windows because GNAT
libraries are unlikely to be on your path.  

Building from Source on Linux
-----------------------------
To build from source, you'll need GNAT FSF 12.1.0 or higher (from GCC). GPRBuild, Ninja, and OpenSSL are also needed. 
GNAT GPL 2021 may also work, but I've begun to use Ada 2022 features so there may be some edge cases.  

Run `ninja` to build the project. Run `ninja destinyunpacker_static` to produce a release build.   
This will produce a build of the program with a "dummy" Oodle SO and no Windows Oodle DLL.
Follow the instructions above to acquire the necessary files.
Please note that WEM files are not compressed - you can stick with the dummy SO for that, but a DLL is required
for the program to launch.  

>	Note: Even though this program is licensed under the GPL, its linking with Oodle makes the actual license unclear.
>	I optionally make this program available under the terms of the LGPL for anyone whom that would assist.

Reference
---------

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
I would have been unable to decompress txtp files on Linux
