#!/bin/sh

TEST=test_xml_AddXMLDeclaration_1
RESULT='<?xml version="1.0" ?>'
./test.sh $TEST "$RESULT"

TEST=test_xml_AddXMLDeclaration_2
RESULT='<?xml version="1.0" encoding="UTF-8" ?>'
./test.sh $TEST "$RESULT"

