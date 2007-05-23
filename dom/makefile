#
default: install
#
OBJFILES= m_strings.o m_dict.o m_dom_debug.o m_dom_error.o \
	  m_dom_types.o m_dom_namednodemap.o m_dom_nodelist.o \
	  m_dom_node.o m_dom_attribute.o m_dom_document.o \
	  m_dom_element.o m_dom_text.o m_dom_parse.o \
	  m_dom_utils.o FoX_dom.o
#
LIBRARY=libFoX_dom.a
INCFLAGS=$(MOD_PREFIX)../objs/finclude
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
	$(AR) $(ARFLAGS_EXTRA) cru $(LIBRARY) $(OBJFILES)
	$(RANLIB) $(LIBRARY)
	$(CP) $(LIBRARY) $(LIB_DIR)
	for i in *.$(MOD_EXT) ; do  \
           $(CP) $$i $(MOD_DIR); done
#
clean:
	rm -f *.o  *.$(MOD_EXT) $(LIBRARY)
# DO NOT DELETE THIS LINE - used by make depend
FoX_dom.o: m_dom_attribute.o m_dom_document.o m_dom_element.o
FoX_dom.o: m_dom_namednodemap.o m_dom_node.o m_dom_nodelist.o m_dom_parse.o
FoX_dom.o: m_dom_text.o m_dom_types.o m_dom_utils.o m_strings.o
m_dict.o: m_strings.o
m_dom_attribute.o: m_dom_node.o m_dom_types.o m_strings.o
m_dom_document.o: m_dom_error.o m_dom_node.o m_dom_types.o m_strings.o
m_dom_documenttype.o: m_dom_types.o
m_dom_element.o: m_dom_attribute.o m_dom_debug.o m_dom_document.o
m_dom_element.o: m_dom_namednodemap.o m_dom_node.o m_dom_nodelist.o
m_dom_element.o: m_dom_types.o m_strings.o
m_dom_implementation.o: m_dom_document.o m_dom_documenttype.o m_dom_node.o
m_dom_namednodemap.o: m_dom_types.o m_strings.o
m_dom_node.o: m_dom_debug.o m_dom_error.o m_dom_namednodemap.o m_dom_nodelist.o
m_dom_node.o: m_dom_types.o m_strings.o
m_dom_nodelist.o: m_dom_types.o
m_dom_parse.o: m_dom_debug.o m_dom_document.o m_dom_element.o
m_dom_parse.o: m_dom_implementation.o m_dom_node.o m_dom_types.o m_strings.o
m_dom_text.o: m_dom_types.o m_strings.o
m_dom_types.o: m_strings.o
m_dom_utils.o: m_dict.o m_dom_namednodemap.o m_dom_node.o m_dom_types.o
m_dom_utils.o: m_strings.o
