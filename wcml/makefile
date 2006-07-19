#
default: install
#
OBJFILES=FoX_wcml.o m_wcml_coma.o m_wcml_core.o m_wcml_stml.o \
         m_wcml_property.o m_wcml_parameter.o m_wcml_metadata.o \
         m_wcml_lattice.o
#
LIBRARY=libFoX_wcml.a
INCFLAGS=$(MOD_PREFIX)../objs/finclude
#
m_wcml_property.f90: m_wcml_quantity.m4
	m4 -DQUANTITY=property -DARGS="(id, title, dictRef, convention, ref, role)" m_wcml_quantity.m4 > $@
m_wcml_parameter.f90: m_wcml_quantity.m4
	m4 -DQUANTITY=parameter -DARGS="(name, id, title, dictRef, convention, ref, role)" m_wcml_quantity.m4 > $@


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
# DO NOT DELETE THIS LINE - used by make depend
