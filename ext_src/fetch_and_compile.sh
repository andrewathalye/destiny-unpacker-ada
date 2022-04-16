#!/bin/sh
git clone https://github.com/zao/ooz.git
mkdir ../ext_lib
mkdir ooz/build
cd ooz/build
cmake .. -G Ninja
ninja liblibooz.so
mv liblibooz.so ../../../ext_lib/
echo "Done"

