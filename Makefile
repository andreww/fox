#
# Example of Makefile for integration of XML package
#
default: objsdir dom_example sax_example wxml_example wcml_example
#
objsdir:
	mkdir -p objs/lib objs/finclude
#
check: dom_check sax_check wxml_check wcml_check
#
include arch.make
#---------------------------
DOM_LIB=dom/libFoX_dom.a
SAX_LIB=sax/libFoX_sax.a
WXML_LIB=wxml/libFoX_wxml.a
WCML_LIB=wcml/libFoX_wcml.a
COMMON_LIB=common/libFoX_common.a
FSYS_LIB=fsys/libfsys.a

INCFLAGS=-Iobjs/finclude
#
DOM_OBJS=dom_example.o
SAX_OBJS=m_handlers.o sax_example.o
WXML_OBJS=wxml_example.o
WCML_OBJS=wcml_example.o
#
dom_lib: sax_lib wxml_lib
	(cd dom; $(MAKE))
dom_lib_clean:
	(cd dom; $(MAKE) clean)
#
sax_lib: common_lib fsys_lib
	(cd sax; $(MAKE))
sax_lib_clean:
	(cd sax; $(MAKE) clean)
#
wxml_lib: fsys_lib
	(cd wxml; $(MAKE))
wxml_lib_clean:
	(cd wxml; $(MAKE) clean)
#
wcml_lib: wxml_lib
	(cd wcml; $(MAKE))
wcml_lib_clean: 
	(cd wcml; $(MAKE) clean)
#
common_lib: fsys_lib
	(cd common; $(MAKE))
common_lib_clean:
	(cd common; $(MAKE) clean)
#
fsys_lib:
	(cd fsys; $(MAKE))
fsys_lib_clean:
	(cd fsys; $(MAKE) clean)
#
dom_example: dom_lib $(DOM_OBJS)
	$(FC) $(LDFLAGS) -o $@ $(DOM_OBJS) $(DOM_LIB) $(SAX_LIB) $(WXML_LIB) $(FSYS_LIB)
#
sax_example: sax_lib $(SAX_OBJS)
	$(FC) $(LDFLAGS) -o $@ $(SAX_OBJS) $(SAX_LIB) $(COMMON_LIB) $(FSYS_LIB)
#
wxml_example: wxml_lib $(WXML_OBJS)
	$(FC) $(LDFLAGS) -o $@ $(WXML_OBJS) $(WXML_LIB) $(COMMON_LIB) $(FSYS_LIB)
#
wcml_example: wcml_lib  $(WCML_OBJS)
	$(FC) $(LDFLAGS) -o $@ $(WCML_OBJS) $(WCML_LIB) $(WXML_LIB) $(FSYS_LIB)
#
dom_check:
sax_check:
wcml_check:
wxml_check:
	(cd wxml/test;./run_tests.sh)
clean: dom_lib_clean sax_lib_clean wxml_lib_clean wcml_lib_clean common_lib_clean fsys_lib_clean
	rm -f *.o dom_example sax_example wxml_example wcml_example simple.xml output.xml *.*d *.a
	rm -f objs/lib/* objs/finclude/*

distclean: clean
	rm -f FoX-config arch.make config.log config.status
