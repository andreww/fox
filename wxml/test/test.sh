#!/bin/sh

make $1.exe
./$1.exe
passed=no
if diff test.xml $1.xml > /dev/null; then
  passed=yes
else
   echo $1 >> failed.out
   diff -u test.xml $1.xml >> failed.out
fi
#rm -f test.xml

if [ $passed = yes ]; then
  echo 'PASSED: ' $1 
  echo 'PASSED: ' $1 >> tests.out
  echo -n '1' >> passed.score
else
  echo 'FAILED: ' $1 
  echo 'FAILED: ' $1 >> tests.out
  echo -n '1' >> failed.score
fi
