#!/bin/sh

eval sed -e 's/TESTINPUT/$1/' test_len_escaping_markup.f90.in > test_len_escaping_markup.f90
make test_len_escaping_markup.exe > /dev/null
OUT=`./test_len_escaping_markup.exe`
rm test_len_escaping_markup.exe test_len_escaping_markup.o test_len_escaping_markup.f90

if [ $OUT = $2 ]; then
  echo Passed: $1
  exit 0
else
  echo Failed: $1
  exit 1
fi
