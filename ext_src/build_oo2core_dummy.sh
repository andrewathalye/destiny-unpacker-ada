#!/bin/sh
mkdir ext_lib
cd ext_src
gcc -c -fpic oo2core_dummy.c
gcc -shared -o liboo2corelinux64.so oo2core_dummy.o
rm oo2core_dummy.o
mv liboo2corelinux64.so ../ext_lib/
