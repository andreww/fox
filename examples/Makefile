#!/usr/bin/make

include ../arch.make

examples: dom_example.exe sax_example.exe wxml_example.exe wcml_example.exe

OBJS = example_xml_module.o
INCFLAGS=$$(../FoX-config --fcflags --wxml)

dom_example.exe: dom_example.o
	$(FC) -g -O2  -ggdb3 -ftrace=full -fbounds-check -flogical=false -freal=nan -fpointer=invalid -finteger=-1 -o $@ $< $$(../FoX-config --libs --dom)
#
sax_example.exe: sax_example.o
	$(FC) -o $@ $< $$(../FoX-config --libs --sax)
#
wxml_example.exe: wxml_example.o 
	$(FC) -o $@ $< $$(../FoX-config --libs --wxml) 
#
wcml_example.exe: wcml_example.o 
	$(FC) -o $@ $< $$(../FoX-config --libs --wcml) 
#
test_small_num.exe: test_small_num.o
	$(FC) -o $@ $< $$(../FoX-config --libs --wxml) 
#
example_xml_program.exe: example_xml_program.o
	$(FC) -o $@ $< $(OBJS) $$(../FoX-config --libs --wxml)
#
example_xml_program.o: $(OBJS)

clean:
	rm -f *.o *.*d *.*D *example
