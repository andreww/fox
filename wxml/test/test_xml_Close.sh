#!/bin/sh -e

for t in test_xml_Close*.f90
do
  TEST=${t%.f90}
  ./test.sh $TEST
done
