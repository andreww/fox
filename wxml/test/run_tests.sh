#!/bin/sh -e

export INCFLAGS=`../../FoX-config --fcflags --wxml`
make clean
rm -f passed.score failed.score
rm -f tests.out failed.out
touch passed.score failed.score

echo "Testing XML File creation"
./test_xml_openFile.sh

echo "Testing XML Processing Instructions"
./test_xml_AddXMLPI.sh

echo "Testing XML Comments"
./test_xml_AddComment.sh

echo "Testing XML NewElement"
./test_xml_NewElement.sh

echo "Testing XML Stylesheet"
./test_xml_AddXMLStylesheet.sh

echo "Testing XML Declaration"
./test_xml_AddXMLDeclaration.sh

echo "Testing XML file closing"
./test_xml_Close.sh


#exit

echo Test Results:
echo Passed: `wc -c passed.score| cut -f 1 -d 'p'`
echo Failed: `wc -c failed.score| cut -f 1 -d 'f'`

echo See failed.out for details of failed tests.
