#!/bin/sh

make $1.exe
output=$(./$1.exe 2>&1)
passed=no
if [ -f $1.xml ]
then
  if diff test.xml $1.xml > /dev/null; then
    passed=yes
  else
     echo $1 >> failed.out
     diff -u test.xml $1.xml >> failed.out
  fi
else
  if echo $output | grep ERROR > /dev/null; then
    passed=yes
  else
     echo $1 >> failed.out
     echo "Failed to fail" >> failed.out
  fi
fi

if [ $passed = yes ]; then
  echo 'PASSED: ' $1 
  echo 'PASSED: ' $1 >> tests.out
  echo -n '1' >> passed.score
else
  echo 'FAILED: ' $1 
  echo 'FAILED: ' $1 >> tests.out
  echo -n '1' >> failed.score
fi
