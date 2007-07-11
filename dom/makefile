#
default: install
#
M4FILES = m_dom_types.m4 m_dom_node.m4 m_dom_nodelist.m4 \
	  m_dom_namednodemap.m4 m_dom_implementation.m4 \
  	  m_dom_document.m4 m_dom_document_type.m4 \
   	  m_dom_element.m4 m_dom_attribute.m4 \
       	  m_dom_character_data.m4 m_dom_entity.m4 \
          m_dom_processing_instruction.m4 m_dom_text.m4 \
 	  m_dom_common.m4 m_dom_exception.m4 m_dom_dom.m4
OBJFILES= m_dom_debug.o m_dom_error.o \
	  m_dom_parse.o m_dom_utils.o \
	  m_dom_dom.o FoX_dom.o
#
LIBRARY=libFoX_dom.a
INCFLAGS=$(MOD_PREFIX)../objs/finclude
#
#---------NOTHING SHOULD NEED TO BE CHANGED BELOW THIS LINE-----
#
include ../arch.make
#
m_dom_utils.f90: m_dom_utils.m4
	m4 -I ../m4 $< > $@
#
m_dom_dom.f90: $(M4FILES)
	m4 -I ../m4 m_dom_dom.m4 > $@ 
#
# Uncomment one of the following lines for debugging/profiling support
#
#FFLAGS=$(FFLAGS_DEBUG)
#FFLAGS=$(FFLAGS_PROFILE)
#FFLAGS=$(FFLAGS_CHECK)
#
CP=cp -p
install: $(OBJFILES)
	@echo "  ==> Updating $(LIBRARY) with $(OBJFILES)"
	$(AR) $(ARFLAGS_EXTRA) cru $(LIBRARY) $(OBJFILES)
	$(RANLIB) $(LIBRARY)
	$(CP) $(LIBRARY) $(LIB_DIR)
	for i in *.$(MOD_EXT) ; do  \
           $(CP) $$i $(MOD_DIR); done
#
clean:
	rm -f *.o  *.$(MOD_EXT) $(LIBRARY)
# DO NOT DELETE THIS LINE - used by make depend
FoX_dom.o: m_dom_dom.o m_dom_parse.o m_dom_utils.o
m_dom_dom.o: m_dom_error.o
m_dom_parse.o: m_dom_debug.o m_dom_dom.o
m_dom_utils.o: m_dom_dom.o
