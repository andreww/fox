#!/bin/make

.SUFFIXES:
.SUFFIXES: .html .md

OBJECTS = wxml.html str.html install.html

default: $(OBJECTS)

.md.html:
	markdown $< > $@
