#!/bin/sh -e

eval sed -e 's_TESTINPUT_$1_' test_str.f90.in > test_str.f90
make test_str.exe > /dev/null 2>&1
OUT=`./test_str.exe`
rm -f test_str.exe test_str.o test_str.f90

echo $OUT $2

if [ $OUT = $2 ]; then
  echo Passed: $2
  exit 0
else
  echo Failed: $2
  exit 1
fi
