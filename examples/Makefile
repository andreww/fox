#!/usr/bin/make

include ../arch.make

default: $(BUILD_TARGETS)

INCFLAGS=`../FoX-config --fcflags`

EXAMPLES=dom_example$(EXEEXT) dom_example_2$(EXEEXT) sax_example$(EXEEXT) sax_example_2$(EXEEXT) wcml_example$(EXEEXT) wxml_example$(EXEEXT)

dom_lib: dom_example$(EXEEXT) dom_example_2$(EXEEXT)
sax_lib: sax_example$(EXEEXT) sax_example_2$(EXEEXT)
wcml_lib: wcml_example$(EXEEXT)
wxml_lib: wxml_example$(EXEEXT)

# Note that we cannot use $< below since IBM make doesn't understand it.

dom_example$(EXEEXT): dom_example.$(OBJEXT) ../objs/lib/libFoX_dom.$(LIBEXT) ../objs/lib/libFoX_sax.$(LIBEXT) ../objs/lib/libFoX_wxml.$(LIBEXT) ../objs/lib/libFoX_common.$(LIBEXT) ../objs/lib/libfsys.$(LIBEXT)
	$(FC) $(FFLAGS) $(LDFLAGS) $(LINK_O_FLAG) $@ dom_example.$(OBJEXT) $$(../FoX-config --libs --dom)
dom_example_2$(EXEEXT): dom_example_2.$(OBJEXT) ../objs/lib/libFoX_dom.$(LIBEXT) ../objs/lib/libFoX_sax.$(LIBEXT) ../objs/lib/libFoX_wxml.$(LIBEXT) ../objs/lib/libFoX_common.$(LIBEXT) ../objs/lib/libfsys.$(LIBEXT)
	$(FC) $(FFLAGS) $(LDFLAGS) $(LINK_O_FLAG) $@ dom_example_2.$(OBJEXT) $$(../FoX-config --libs --dom)
#
sax_example$(EXEEXT): sax_example.$(OBJEXT) ../objs/lib/libFoX_sax.$(LIBEXT) ../objs/lib/libFoX_common.$(LIBEXT) ../objs/lib/libfsys.$(LIBEXT)
	$(FC) $(FFLAGS) $(LDFLAGS) $(LINK_O_FLAG) $@ sax_example.$(OBJEXT) $$(../FoX-config --libs --sax)
sax_example_2$(EXEEXT): sax_example_2.$(OBJEXT) ../objs/lib/libFoX_sax.$(LIBEXT) ../objs/lib/libFoX_common.$(LIBEXT) ../objs/lib/libfsys.$(LIBEXT)
	$(FC) $(FFLAGS) $(LDFLAGS) $(LINK_O_FLAG) $@ sax_example_2.$(OBJEXT) $$(../FoX-config --libs --sax)
#
wxml_example$(EXEEXT): wxml_example.$(OBJEXT) ../objs/lib/libFoX_wxml.$(LIBEXT) ../objs/lib/libFoX_common.$(LIBEXT) ../objs/lib/libfsys.$(LIBEXT)
	$(FC) $(FFLAGS) $(LDFLAGS) $(LINK_O_FLAG) $@ wxml_example.$(OBJEXT) $$(../FoX-config --libs --wxml)
#
wcml_example$(EXEEXT): wcml_example.$(OBJEXT)  ../objs/lib/libFoX_wcml.$(LIBEXT) ../objs/lib/libFoX_wxml.$(LIBEXT) ../objs/lib/libFoX_common.$(LIBEXT) ../objs/lib/libfsys.$(LIBEXT)
	$(FC) $(FFLAGS) $(LDFLAGS) $(LINK_O_FLAG) $@ wcml_example.$(OBJEXT) $$(../FoX-config --libs --wcml)

clean:
	rm -f *.$(OBJEXT) *.$(MOD_EXT) $(EXAMPLES)
