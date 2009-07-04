#!/bin/sh

INCFLAGS=`../../FoX-config --fcflags`
export INCFLAGS
rm -f passed.score failed.score
rm -f tests.out failed.out
touch passed.score failed.score

for t in test_ncml?*.sh
do
  ./$t
done

echo RESULT wncml/ Test Results:
echo RESULT wncml/ Passed: `wc -l passed.score| cut -f 1 -d 'p'`
echo RESULT wncml/ Failed: `wc -l failed.score| cut -f 1 -d 'f'`

echo RESULT wncml/ See wncml/test/failed.out for details of failed tests.
