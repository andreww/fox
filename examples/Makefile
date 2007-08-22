#!/usr/bin/make

include ../arch.make

examples: dom_example.exe sax_example.exe wxml_example.exe wcml_example.exe

OBJS = example_xml_module.o
INCFLAGS=$$(../FoX-config --fcflags --wxml)

# Note that we cannot use $< below since IBM make doesn't understand it.

dom_example.exe: dom_example.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ dom_eample.o $$(../FoX-config --libs --dom)
#
sax_example.exe: sax_example.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ sax_example.o $$(../FoX-config --libs --sax)
#
wxml_example.exe: wxml_example.o 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ wxml_example.o $$(../FoX-config --libs --wxml) 
#
wcml_example.exe: wcml_example.o 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ wcml_example.o $$(../FoX-config --libs --wcml) 
#
example_xml_program.o: $(OBJS)

clean:
	rm -f *.o *.*d *.*D *example
