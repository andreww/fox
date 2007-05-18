#!/bin/sh

# NB Note that we ensure all locally-produced files 
# have Unix line endings only by using 'tr', in
# order to compare properly to our canonical versions.

make $1.exe
./$1.exe 2>&1 | tr -d '\15' > test.out
passed=no
if [ -f $1.xml ]
then
  grep -v UUID test.xml | tr -d '/15' > test.xml.tmp; mv test.xml.tmp test.xml
  if diff test.xml $1.xml > /dev/null; then
     echo $1 >> failed.out
     echo "------------" >> failed.out
     diff test.xml $1.xml >> failed.out
     echo "------------" >> failed.out
  else
    passed=yes
  fi
elif [ -f $1.out ]
then
# Note below that we don't do a direct grep; we just check 
# that the only DIFFerences are in one direction.
  if diff test.out $1.out | grep "^>" > /dev/null; then
     echo $1 >> failed.out
     echo "------------" >> failed.out
     diff test.out $1.out >> failed.out
     echo "------------" >> failed.out
  else
    passed=yes
  fi
else
  echo No test output found for $1
fi

if [ $passed = yes ]; then
  echo 'PASSED: ' $1 
  echo 'PASSED: ' $1 >> tests.out
  echo '1' >> passed.score
else
  echo 'FAILED: ' $1 
  echo 'FAILED: ' $1 >> tests.out
  echo '1' >> failed.score
fi
