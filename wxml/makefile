#
default: install
#
OBJFILES= m_wxml_escape.$(OBJEXT) m_wxml_core.$(OBJEXT) \
          m_wxml_overloads.$(OBJEXT) FoX_wxml.$(OBJEXT)
#
LIBRARY=libFoX_wxml.$(LIBEXT)
INCFLAGS=$(MOD_PREFIX)../objs/finclude
#
.m4.f90:
	m4 -I ../m4 $< > $@

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
	@echo "==> Creating $(LIBRARY) with $(OBJFILES)"
	$(AR) $(ARFLAGS_EXTRA) cru $(LIBRARY) $(OBJFILES)
	$(RANLIB) $(LIBRARY)
	$(CP) $(LIBRARY) $(LIB_DIR)
	for i in *.$(MOD_EXT); do \
	  $(CP) $$i $(MOD_DIR); done
#
check:
	(cd test; make)
#
clean:
	rm -f *.$(OBJEXT) *.$(MOD_EXT) $(LIBRARY)

m_wxml_overloads.f90: m_wxml_overloads.m4
# DO NOT DELETE THIS LINE - used by make depend
FoX_wxml.$(OBJEXT): m_wxml_core.$(OBJEXT) m_wxml_overloads.$(OBJEXT)
m_wxml_core.$(OBJEXT): m_wxml_escape.$(OBJEXT)
m_wxml_overloads.$(OBJEXT): m_wxml_core.$(OBJEXT)
