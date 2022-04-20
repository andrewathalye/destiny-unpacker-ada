Destiny Linux Unpacker 
======================

This is a fully-functional Linux unpacker for Destiny 1 and 2, written in Ada.  
It makes use of Linoodle (https://www.github.com/McSimp/linoodle) in order to
interface with Oodle, the compressor used by Destiny.

Note: To comply with the terms of the Bungie EULA, only use this program for personal
purposes and ensure that you do not publish the contents of any Destiny package files
without Bungie's explicit permission.

Setup and Usage
---------------

If you would like to build from source, see that section. Otherwise,
download a release from the Releases section (if available).

Destiny 1 and 2 packages are compressed with a library called Oodle, which is
distributed under an all rights reserved license and thus cannot be included.

In order to decompress files, do the following:
	- If you intend to use Destiny 1 or Destiny 2 before Beyond Light,
	acquire oo2core_3_win64.dll and place it in the same directory as the tool.
	- If you intend to use Destiny 2 after Beyond Light,
	acquire oo2corelinux64.so.9 and replace the dummy file in ext_lib.
To acquire oo2core_3_win64.dll, just look inside the bin directory of the Destiny 2 Shadowkeep
depot folder.
To acquire oo2corelinux64.so.9, follow the instructions at https://github.com/gildor2/UEViewer/tree/master/libs/oodle.
>	Note: This program is for use for educational purposes only.
>	RAD Game Tools, Epic Game Tools, etc. reserve the right to define the terms under which users may
>	use Oodle and related software, to which they own the copyright.
>	If unsure, you might consider purchasing a license to Oodle 2.9.

The program is used as follows:
`./destinyunpacker [d1, prebl, or postbl] PACKAGES_DIRECTORY OUTPUT_DIRECTORY`

Building from Source
--------------------

To build from source, you'll need GNAT 2021 (FSF or GPL).
Use `./ext_src/fetch_and_compile.sh` to setup linoodle (it requires Ninja, and CMake)
OpenSSL (libcrypto) is needed for encryption. This can be manually removed if you only need D1 support.
Finally, run `gprbuild -Pdestiny_unpacker` to build an executable.

Follow the instructions above to acquire Oodle support. This is technically not necessary if you only want to extract WEM files.
If you do not wish to include oo2corelinux64.so.9, make a blank so file containing the symbol "OodleLZ_Decompress" or simply comment out the calls in unpacker-worker.adb.
Doing this will effectively remove post-Beyond Light support, however it may be necessary if you are unable to acquire the necessary libraries.

Note: Even though this program is licensed under the GPL, its linking with Oodle makes the actual license unclear.
I optionally make this program available under the terms of the LGPL for anyone whom that would assist.

Credit
------

Thank you to nblockbuster and MontagueM, without whose work on DestinyUnpackerCPP this
project would never have been possible.

https://www.github.com/nblockbuster/DestinyUnpackerCPP and
https://www.github.com/MontagueM/DestinyUnpackerCPP

Additionally, I give my thanks to McSimp and https://github.com/McSimp/linoodle, without which
I would have been unable to decompress txtp files on Linux
