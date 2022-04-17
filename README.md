Destiny Linux Unpacker 
======================

This is a work-in-progress Linux unpacker for Destiny 1 and 2, written in Ada.  
It makes use of Linoodle (https://www.github.com/McSimp/linoodle) in order to
interface with Oodle, the compressor used by Destiny. If you are aware of a native
version of Oodle, please let me know - this wrapper is not quite 100% compatible with
all blocks.

Notice
------

The program is completed, but an apparent bug in Linoodle or my pointer code is causing
certain package files to fail to decompress and crash the program. I've checked the size,
position, addresses, and decrypted results, and they seem to match with Windows.

If you think you can help, please get in touch via Pull Requests or Issues to let me know what
works. I suspect the issue is somewhere in Linoodle - maybe it doesn't implement quite enough of the Windows
API to handle every outcome for Oodle's decompression, but I am not well-versed in Windows debugging so I can't
be sure.

Setup and Usage
---------------

If you would like to build from source, see that section. Otherwise,
download a release from the Releases section (if available).

You need to copy the file "oo2core_X_win64.dll" into the same folder as the executable.
X is 3 for Shadowkeep and Destiny 1, and 8 for Beyond Light and later versions.
There are various ways to obtain this file, including downloading a Shadowkeep / Beyond Light build of Destiny 2.

The program is used as follows:
`./destinyunpacker [d1, prebl, or postbl] PACKAGES_DIRECTORY OUTPUT_DIRECTORY`

Building from Source
--------------------

To build from source, you'll need GNAT 2021 (FSF or GPL).
Use `./ext_src/fetch_and_compile.sh` to setup linoodle (it requires Ninja, and CMake)
OpenSSL (libcrypto) is needed for encryption. This can be manually removed if you only need D1 support.
Finally, run `gprbuild -Pdestiny_unpacker` to build an executable.

Note: Even though this program is licensed under the GPL, its linking with Oodle makes the actual license somewhat murky.
I optionally make this program available under the terms of the LGPL for anyone whom that would assist.

Credit
------

Thank you to nblockbuster and MontagueM, without whose work on DestinyUnpackerCPP this
project would never have been possible.

https://www.github.com/nblockbuster/DestinyUnpackerCPP and
https://www.github.com/MontagueM/DestinyUnpackerCPP
