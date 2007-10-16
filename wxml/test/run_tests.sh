#!/bin/sh

export INCFLAGS=`../../FoX-config --fcflags --wxml`
make clean
rm -f passed.score failed.score
rm -f tests.out failed.out
touch passed.score failed.score

for t in test_xml_*.sh
do
  ./$t
done

echo Test Results:
echo Passed: `grep -c PASSED tests.out`
echo Failed: `grep -c FAILED tests.out`

echo See failed.out for details of failed tests.
