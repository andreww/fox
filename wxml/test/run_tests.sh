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

#exit

echo Test Results:
echo Passed: `wc -c passed.score`
echo Failed: `wc -c failed.score`

echo See failed.out for details of failed tests.
