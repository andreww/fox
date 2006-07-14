#
default: install
#
OBJFILES=m_common_array_str.o m_common_attrs.o m_common_buffer.o \
         m_common_format.o m_common_charset.o m_common_namespaces.o\
         m_common_error.o m_common_elstack.o m_common_io.o FoX_common.o \
	 m_common_namecheck.o
#
LIBRARY=libFoX_common.a
INCFLAGS=$(MOD_PREFIX)../objs/finclude
#
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
	$(CP) $(LIBRARY) $(LIB_DIR) 
	for i in *.$(MOD_EXT); do \
      	    $(CP) $$i $(MOD_DIR); done
#
clean:
	rm -f *.o  *.$(MOD_EXT) $(LIBRARY)
# DO NOT DELETE THIS LINE - used by make depend
FoX_common.o: m_common_attrs.o
m_common_attrs.o: m_common_array_str.o m_common_charset.o m_common_error.o
m_common_buffer.o: m_common_error.o
m_common_elstack.o: m_common_error.o
m_common_io.o: m_common_error.o
m_common_namespaces.o: FoX_common.o m_common_array_str.o m_common_error.o
