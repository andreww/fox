#
default: install
#
OBJFILES=FoX_wcml.o m_wcml_coma.o m_wcml_core.o m_wcml_stml.o \
         m_wcml_parameter.o m_wcml_property.o m_wcml_metadata.o \
         m_wcml_lattice.o m_wcml_geometry.o m_wcml_molecule.o \
         m_wcml_lists.o
#
LIBRARY=libFoX_wcml.a
INCFLAGS=$(MOD_PREFIX)../objs/finclude
#
.m4.f90:
	m4 -I ../m4 $< > $@
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
m_wcml_coma.f90: m_wcml_coma.m4
m_wcml_geometry.f90: m_wcml_geometry.m4
m_wcml_lattice.f90: m_wcml_lattice.m4
m_wcml_lists.f90: m_wcml_lists.m4
m_wcml_molecule.f90: m_wcml_molecule.m4
m_wcml_parameter.f90: m_wcml_parameter.m4 ../m4/quantity.m4 ../m4/foreach.m4 ../m4/datatypes.m4 ../m4/common.m4
m_wcml_property.f90: m_wcml_property.m4 ../m4/quantity.m4 ../m4/foreach.m4 ../m4/datatypes.m4 ../m4/common.m4 
m_wcml_stml.f90: m_wcml_stml.m4 ../m4/foreach.m4 ../m4/common.m4 ../m4/datatypes.m4
# DO NOT DELETE THIS LINE - used by make depend
FoX_wcml.o: m_wcml_coma.o m_wcml_core.o m_wcml_geometry.o m_wcml_lattice.o
FoX_wcml.o: m_wcml_lists.o m_wcml_metadata.o m_wcml_molecule.o
FoX_wcml.o: m_wcml_parameter.o m_wcml_property.o m_wcml_stml.o
m_wcml_coma.o: m_wcml_stml.o
m_wcml_core.o: m_wcml_metadata.o
m_wcml_parameter.o: m_wcml_stml.o
m_wcml_property.o: m_wcml_stml.o
