Destiny Linux Unpacker 
======================

This is a work-in-progress Linux unpacker for Destiny 1 and 2, written in Ada.  
It makes use of OOZ (https://github.com/zao/ooz) in order to
interface with Oodle, the compressor used by Destiny.

Setup and Usage
---------------

If you would like to build from source, see that section. Otherwise,
download a release from the Releases section (if available).

The program is used as follows:
`./destinyunpacker [d1, prebl, or postbl] PACKAGES_DIRECTORY OUTPUT_DIRECTORY`

Building from Source
--------------------

To build from source, you'll need GNAT 2021 (FSF or GPL).
Use `./ext_src/fetch_and_compile.sh` to setup ooz (it requires libsodium, Ninja, and CMake)
OpenSSL (libcrypto) is needed for encryption. This can be manually removed if you only need D1 support.
Finally, run `gprbuild -Pdestiny_unpacker` to build an executable.

Credit
------

Thank you to nblockbuster and MontagueM, without whose work on DestinyUnpackerCPP this
project would never have been possible.

https://www.github.com/nblockbuster/DestinyUnpackerCPP and
https://www.github.com/MontagueM/DestinyUnpackerCPP
