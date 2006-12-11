#!/bin/sh -e

export INCFLAGS=`../../FoX-config --fcflags --wxml`
make clean
rm -f passed.score failed.score
rm -f tests.out failed.out
touch passed.score failed.score

for t in test_sax*.sh
do
  ./$t
done

echo Test Results:
echo Passed: `wc -l passed.score| cut -f 1 -d 'p'`
echo Failed: `wc -l failed.score| cut -f 1 -d 'f'`

echo See failed.out for details of failed tests.
