#!/bin/sh
mkdir ext_lib
cd ext_src
gcc -shared -fpic -Wall -Werror -pedantic -o liboo2corelinux64.so oo2core_dummy.c
mv liboo2corelinux64.so ../ext_lib/
