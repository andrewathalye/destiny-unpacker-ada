#!/bin/sh
git clone https://github.com/McSimp/linoodle.git --recursive
mkdir ../ext_lib
mkdir linoodle/build
cd linoodle/build
cmake .. -G Ninja
ninja liblinoodle.so
mv liblinoodle.so ../../../ext_lib/
echo "Done"

