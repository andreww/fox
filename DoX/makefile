#!/bin/make

.SUFFIXES:
.SUFFIXES: .html .md

OBJECTS = wxml.html str.html

default: $(OBJECTS)

.md.html:
	markdown $< > $@
