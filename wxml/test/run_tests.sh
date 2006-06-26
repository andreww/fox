#!/bin/sh -e

export INCFLAGS=`../../FoX-config --fcflags --wxml`
make clean
rm -f passed.score failed.score
rm -f tests.out failed.out

echo "Testing XML File creation"
./test_xml_openFile.sh

echo "Testing XML Stylesheet"
./test_xml_AddXMLStylesheet.sh

echo "Testing XML Declaration"
./test_xml_AddXMLDeclaration.sh

exit

#Cannot do tests below until we have well-specified
#numerical output

echo "Testing integer to string conversion"

for i in '0 0' '1 1' '-1 -1' '10 10' '-356 -356'
do
  ./test_str.sh $i
done

echo "Testing logical to string conversion"

for i in '.True. true' '.False. false';
do
  ./test_str.sh $i
done

echo "Testing single precision to string conversion"

for i in '0.00 0.00' '-0.00 -0.00' '0.125 -0.125'
do
  ./test_str.sh $i
done

echo "Testing len_escaping_markup"

for i in '\"h\" 1' '\"\&\" 5'
do
  ./test_len_escaping_markup.sh $i
done

echo Test Results:
echo Passed: `wc -c passed.score`
echo Failed: `wc -c failed.score`

echo See failed.out for details of failed tests.
