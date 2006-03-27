#!/bin/sh -e

eval sed -e 's/TESTINPUT/$1/' test_str.f90.in > test_str.f90
make test_str.exe
OUT=`./test_str.exe`
rm -f test_str.exe test_str.o test_str.f90

if [ $OUT = $2 ]; then
  echo Passed: $OUT
  exit 0
else
  echo Failed: $OUT
  exit 1
fi
