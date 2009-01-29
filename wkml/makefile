#
default: install
#
OBJFILES=FoX_wkml.o m_wkml_lowlevel.o m_wkml_color.o m_wkml_styling.o m_wkml_features.o m_wkml_coverage.o m_wkml_core.o m_wkml_contours.o m_contours.o m_wkml_color_def.o m_wkml_chart.o
#
LIBRARY=libFoX_wkml.a
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
# DO NOT DELETE THIS LINE - used by make depend
FoX_wkml.o: m_wkml_color.o m_wkml_contours.o m_wkml_core.o m_wkml_coverage.o
FoX_wkml.o: m_wkml_features.o m_wkml_lowlevel.o m_wkml_styling.o m_wkml_chart.o
m_wkml_color.o: m_wkml_color_def.o
m_wkml_contours.o: m_contours.o m_wkml_color.o m_wkml_features.o
m_wkml_contours.o: m_wkml_lowlevel.o m_wkml_styling.o
m_wkml_core.o: m_wkml_color.o m_wkml_lowlevel.o m_wkml_styling.o
m_wkml_coverage.o: m_wkml_color.o m_wkml_core.o m_wkml_lowlevel.o
m_wkml_coverage.o: m_wkml_styling.o m_wkml_chart.o
m_wkml_features.o: m_wkml_color.o m_wkml_core.o m_wkml_lowlevel.o
m_wkml_features.o: m_wkml_styling.o m_wkml_chart.o
m_wkml_lowlevel.o: m_wkml_color.o
m_wkml_styling.o: m_wkml_color.o m_wkml_lowlevel.o
m_wkml_chart.o: m_wkml_lowlevel.o
