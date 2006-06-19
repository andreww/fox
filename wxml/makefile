#
default: install
#
OBJFILES= m_wxml_array_str.o m_wxml_dictionary.o \
          m_wxml_text.o m_wxml_escape.o m_wxml_core.o \
          m_wxml_overloads.o FoX_wxml.o m_wxml_error.o
#
LIBRARY=libFoX_wxml.a
INCFLAGS=$(MOD_PREFIX)../objs/finclude
#
#---------NOTHING SHOULD NEED TO BE CHANGED BELOW THIS LINE-----
#
include ../arch.make

# Uncomment one of the following lines for debugging/profiling support
#
#FFLAGS=$(FFLAGS_DEBUG)
#FFLAGS=$(FFLAGS_PROFILE)
#FFLAGS=$(FFLAGS_CHECK)
#
CP=cp -p
install: $(OBJFILES)
	$(AR) $(ARFLAGS_EXTRA) cru $(LIBRARY) $(OBJFILES)
	$(RANLIB) $(LIBRARY)
	$(CP) $(LIBRARY) $(LIB_DIR)
	for i in *.$(MOD_EXT); do \
	  $(CP) $$i $(MOD_DIR); done
#
clean:
	rm -f *.o  *.$(MOD_EXT) $(LIBRARY)
# DO NOT DELETE THIS LINE - used by make depend
FoX_wxml.o: m_wxml_core.o m_wxml_overloads.o m_wxml_text.o
m_wxml_core.o: m_wxml_array_str.o m_wxml_dictionary.o m_wxml_escape.o
m_wxml_core.o: m_wxml_text.o
m_wxml_dictionary.o: m_wxml_array_str.o m_wxml_error.o m_wxml_escape.o
m_wxml_overloads.o: m_wxml_core.o m_wxml_text.o
