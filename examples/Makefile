#!/usr/bin/make

include ../arch.make

examples: sax_example wxml_example wcml_example

OBJS = example_xml_module.o
INCFLAGS=$$(../FoX-config --fcflags --wxml)

dom_example: dom_example.o
	$(FC) -o $@ dom_example.o $$(../FoX-config --libs --dom)
#
sax_example: sax_example.o
	$(FC) -o $@ sax_example.o $$(../FoX-config --libs --sax)
#
wxml_example: wxml_example.o 
	$(FC) -o $@ wxml_example.o $$(../FoX-config --libs --wxml) 
#
wcml_example: wcml_example.o 
	$(FC) -o $@ wcml_example.o $$(../FoX-config --libs --wcml) 
#
example_xml_program: example_xml_program.o
	$(FC) -o $@ $< $(OBJS) $$(../FoX-config --libs --wxml)
#
example_xml_program.o: $(OBJS)

clean:
	rm -f *.o *.*d *.*D *example
