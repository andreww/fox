#!/usr/bin/make

include ../arch.make

default: $(BUILD_TARGETS)

EXAMPLES=dom_example$(EXEEXT) dom_example_2$(EXEEXT) sax_example$(EXEEXT) sax_example_2$(EXEEXT) wcml_example$(EXEEXT) wxml_example$(EXEEXT)

dom_lib: dom_example$(EXEEXT) dom_example_2$(EXEEXT)
sax_lib: sax_example$(EXEEXT) sax_example_2$(EXEEXT)
wcml_lib: wcml_example$(EXEEXT)
wxml_lib: wxml_example$(EXEEXT)

# Note that we cannot use $< below since IBM make doesn't understand it.

dom_example$(EXEEXT): dom_example.$(OBJEXT) ../objs/lib/libFoX_dom.a ../objs/lib/libFoX_sax.a ../objs/lib/libFoX_wxml.a ../objs/lib/libFoX_common.a
	$(FC) $(FFLAGS) $(LDFLAGS) -o $* dom_example.$(OBJEXT) $$(../FoX-config --libs --dom)
dom_example_2$(EXEEXT): dom_example_2.$(OBJEXT) ../objs/lib/libFoX_dom.a ../objs/lib/libFoX_sax.a ../objs/lib/libFoX_wxml.a ../objs/lib/libFoX_common.a
	$(FC) $(FFLAGS) $(LDFLAGS) -o $* dom_example_2.$(OBJEXT) $$(../FoX-config --libs --dom)
#
sax_example$(EXEEXT): sax_example.$(OBJEXT) ../objs/lib/libFoX_sax.a ../objs/lib/libFoX_common.a
	$(FC) $(FFLAGS) $(LDFLAGS) -o $* sax_example.$(OBJEXT) $$(../FoX-config --libs --sax)
sax_example_2$(EXEEXT): sax_example_2.$(OBJEXT) ../objs/lib/libFoX_sax.a
	$(FC) $(FFLAGS) $(LDFLAGS) -o $* sax_example_2.$(OBJEXT) $$(../FoX-config --libs --sax)
#
wxml_example$(EXEEXT): wxml_example.$(OBJEXT) ../objs/lib/libFoX_wxml.a ../objs/lib/libFoX_common.a
	$(FC) $(FFLAGS) $(LDFLAGS) -o $* wxml_example.$(OBJEXT) $$(../FoX-config --libs --wxml)
#
wcml_example$(EXEEXT): wcml_example.$(OBJEXT)  ../objs/lib/libFoX_wcml.a ../objs/lib/libFoX_wxml.a ../objs/lib/libFoX_common.a
	$(FC) $(FFLAGS) $(LDFLAGS) -o $* wcml_example.$(OBJEXT) $$(../FoX-config --libs --wcml)

clean:
	rm -f *.$(OBJEXT) *.$(MOD_EXT) $(EXAMPLES)
