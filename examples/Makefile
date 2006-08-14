#!/usr/bin/make

include ../arch.make

OBJS = example_xml_module.o
INCFLAGS=$$(../FoX-config --fcflags --wxml)

example_xml_program: example_xml_program.o
	$(FC) -o $@ $< $(OBJS) $$(../FoX-config --libs --wxml)

example_xml_program.o: $(OBJS)
