#!/usr/bin/make

include ../arch.make

INCFLAGS=$$(../FoX-config --fcflags --wxml)

default: $(BUILD_TARGETS)

dom_lib: dom_example.exe dom_example_2.exe
sax_lib: sax_example.exe sax_example_2.exe
wcml_lib: wcml_example.exe
wxml_lib: wxml_example.exe

# Note that we cannot use $< below since IBM make doesn't understand it.

dom_example.exe: dom_example.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ dom_example.o $$(../FoX-config --libs --dom)
dom_example_2.exe: dom_example_2.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ dom_example_2.o $$(../FoX-config --libs --dom)
#
sax_example.exe: sax_example.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ sax_example.o $$(../FoX-config --libs --sax)
sax_example_2.exe: sax_example_2.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ sax_example_2.o $$(../FoX-config --libs --sax)
#
wxml_example.exe: wxml_example.o 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ wxml_example.o $$(../FoX-config --libs --wxml) 
#
wcml_example.exe: wcml_example.o 
	$(FC) $(FFLAGS) $(LDFLAGS) -o $@ wcml_example.o $$(../FoX-config --libs --wcml) 

clean:
	rm -f *.o *.*d *.*D *.exe
