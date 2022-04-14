Destiny Linux Unpacker 
======================

This is a work-in-progress Linux unpacker for Destiny 1 and 2, written in Ada.  
It makes use of Linoodle (https://www.github.com/McSimp/linoodle) in order to
interface with Oodle, the compressor used by Destiny.

Setup and Usage
---------------

Copy an Oodle DLL from the game and rename it oo2core_8_win64.dll.
This DLL must be placed in the root folder of the project.

If you would like to build from source, see that section. Otherwise,
download a release from the Releases section (if available).

The program is used as follows:
`./destinyunpacker [d1, prebl, or postbl] PACKAGES_DIRECTORY OUTPUT_DIRECTORY`

Building from Source
--------------------

To build from source, you'll need GNAT 2021 (FSF or GPL).
Use `./ext_src/fetch_and_compile.sh` to setup linoodle and put it in ext_lib (it requires Ninja and CMake).
Finally, run `gprbuild -Pdestiny_unpacker` to build an executable.

Credit
------

Thank you to nblockbuster and MontagueM, without whose work on DestinyUnpackerCPP this
project would never have been possible.

https://www.github.com/nblockbuster/DestinyUnpackerCPP and
https://www.github.com/MontagueM/DestinyUnpackerCPP
