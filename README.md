Destiny Linux Unpacker 
======================

This is a work-in-progress Linux unpacker for Destiny 1 and 2, written in Ada.  
It makes use of Linoodle (https://www.github.com/McSimp/linoodle) in order to
interface with Oodle, the compressor used by Destiny.

Setup and Usage
---------------

If you would like to build from source, see that section. Otherwise,
download a release from the Releases section (if available).

You need to copy the file "oo2core_8_win64.dll" into the same folder as the executable.
There are various ways to obtain this file, including downloading a Beyond Light build of Destiny 2.

The program is used as follows:
`./destinyunpacker [d1, prebl, or postbl] PACKAGES_DIRECTORY OUTPUT_DIRECTORY`

Building from Source
--------------------

To build from source, you'll need GNAT 2021 (FSF or GPL).
Use `./ext_src/fetch_and_compile.sh` to setup linoodle (it requires Ninja, and CMake)
OpenSSL (libcrypto) is needed for encryption. This can be manually removed if you only need D1 support.
Finally, run `gprbuild -Pdestiny_unpacker` to build an executable.

Credit
------

Thank you to nblockbuster and MontagueM, without whose work on DestinyUnpackerCPP this
project would never have been possible.

https://www.github.com/nblockbuster/DestinyUnpackerCPP and
https://www.github.com/MontagueM/DestinyUnpackerCPP
