#
default: install
#
OBJFILES=m_pxf_abort_flush.$(OBJEXT) pxf.$(OBJEXT) \
         fox_m_fsys_array_str.$(OBJEXT) \
         fox_m_fsys_format.$(OBJEXT) \
         fox_m_fsys_parse_input.$(OBJEXT) \
         fox_m_fsys_string.$(OBJEXT) \
         fox_m_fsys_string_list.$(OBJEXT) \
         fox_m_fsys_realtypes.$(OBJEXT)
#
LIBRARY=libfsys.$(LIBEXT)
INCFLAGS=

.m4.F90:
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
	@echo "  ==> Creating $(LIBRARY) with $(OBJFILES)"
	@$(AR) $(ARFLAGS_EXTRA) cru $(LIBRARY) $(OBJFILES)
	@$(RANLIB) $(LIBRARY)
	$(CP) $(LIBRARY) $(LIB_DIR) 
	for i in *.$(MOD_EXT); do \
      	    $(CP) $$i $(MOD_DIR); done
#
clean:
	rm -f *.$(OBJEXT) *.$(MOD_EXT) $(LIBRARY)
fox_m_fsys_format.$(OBJEXT): fox_m_fsys_realtypes.$(OBJEXT) pxf.$(OBJEXT)
fox_m_fsys_parse_input.$(OBJEXT): fox_m_fsys_realtypes.$(OBJEXT)
fox_m_fsys_string_list.$(OBJEXT): fox_m_fsys_array_str.$(OBJEXT)
