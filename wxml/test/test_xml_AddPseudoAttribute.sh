#!/bin/sh -e

for t in test_xml_AddPseudoAttribute*.f90
do
  TEST=${t%.f90}
  ./test.sh $TEST
done
