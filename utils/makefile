#
default: install
#
OBJFILES=FoX_utils.$(OBJEXT) fox_m_utils_mtprng.$(OBJEXT) fox_m_utils_uuid.$(OBJEXT)
#
LIBRARY=libFoX_utils.$(LIBEXT)
INCFLAGS=$(MOD_PREFIX)../objs/finclude
#
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
	rm -f *.$(OBJEXT)  *.$(MOD_EXT) $(LIBRARY)
# DO NOT DELETE THIS LINE - used by make depend
FoX_utils.$(OBJEXT): fox_m_utils_uuid.$(OBJEXT)
fox_m_utils_uuid.$(OBJEXT): fox_m_utils_mtprng.$(OBJEXT)
