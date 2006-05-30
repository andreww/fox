#
default: install
#
OBJFILES= m_debug.o m_buffer.o m_elstack.o m_dictionary.o m_io.o \
          m_reader.o m_converters.o m_charset.o  m_entities.o m_array_str.o\
          m_sax_namespaces.o m_sax_error.o m_sax_fsm.o m_sax_parser.o \
          FoX_sax.o
#
LIBRARY=libFoX_sax.a
INCFLAGS=$(MOD_PREFIX)../objs/finclude
#

m_reader.o: m_io.o m_debug.o
m_elstack.o: m_buffer.o
m_dictionary.o: m_array_str.o m_sax_error.o
m_entities.o: m_buffer.o m_sax_error.o
m_converters.o: m_debug.o
m_sax_fsm.o: m_buffer.o m_dictionary.o m_charset.o m_entities.o m_elstack.o
m_sax_error.o: m_elstack.o
m_sax_parser.o: m_buffer.o m_elstack.o m_reader.o m_sax_fsm.o m_dictionary.o \
                m_debug.o m_sax_error.o
#
FoX_sax.o: m_dictionary.o m_sax_parser.o m_converters.o m_sax_error.o

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
	for i in *.$(MOD_EXT) ; do  \
           $(CP) $$i $(MOD_DIR);done
#
clean:
	rm -f *.o  *.$(MOD_EXT) $(LIBRARY)
