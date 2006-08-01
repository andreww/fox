#!/bin/sh

make $1.exe
./$1.exe > test.out 2>&1
passed=no
if [ -f $1.xml ]
then
  if diff test.xml $1.xml > /dev/null; then
    passed=yes
  else
     echo $1 >> failed.out
     echo "------------" >> failed.out
     diff -u test.xml $1.xml >> failed.out
     echo "------------" >> failed.out
  fi
elif [ -f $1.out ]
then
  if diff test.out $1.out > /dev/null; then
    passed=yes
  else
     echo $1 >> failed.out
     echo "------------" >> failed.out
     diff -u test.out $1.out >> failed.out
     echo "------------" >> failed.out
  fi
else
  echo No test output found for $1
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
