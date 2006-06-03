#
default: install
#
OBJFILES=FoX_wcml.o m_wcml_coma.o m_wcml_core.o m_wcml_stml.o m_wcml_property.o
#
LIBRARY=libFoX_wcml.a
INCFLAGS=$(MOD_PREFIX)../objs/finclude
#
m_wcml_core.o: m_wcml_stml.o m_wcml_coma.o
m_wcml_coma.o: m_wcml_stml.o
m_wcml_property.o: m_wcml_stml.o
FoX_wcml.o: m_wcml_core.o m_wcml_coma.o m_wcml_stml.o m_wcml_property.o

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
	@echo "  ==> Updating $(LIBRARY) with $(OBJFILES)"
	@$(AR) $(ARFLAGS_EXTRA) cru $(LIBRARY) $(OBJFILES)
	@$(RANLIB) $(LIBRARY)
	$(CP) $(LIBRARY) $(LIB_DIR) ;
	for i in *.$(MOD_EXT) ; do  \
           $(CP) $$i $(MOD_DIR); done
#
clean:
	rm -f *.o  *.$(MOD_EXT) $(LIBRARY)
