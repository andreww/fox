#!/bin/make

.SUFFIXES:
.SUFFIXES: .html .md

OBJECTS = wxml.html

default: $(OBJECTS)

.md.html:
	markdown $< > $@
