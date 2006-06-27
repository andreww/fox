module FoX_sax

!
! Stub module to gather all the functionality needed by the user
! 
! In future m_converters could be exported by
! other parts of a more general fortran library.
! m_converters is not used at all currently.
!
! m_xml_error is necessary in order to use a custom error handler.
!
use FoX_common
use m_sax_parser
use m_sax_converters
use m_sax_error

implicit none
private

public :: open_xmlFile
public :: close_xmlFile
public :: xml_parse

public :: xml_t

!public :: begin_document_handler
!public :: end_document_handler
!public :: begin_element_handler
!public :: end_element_handler
!public :: start_prefix_handler
!public :: end_prefix_handler
!public :: pcdata_chunk_handler
!public :: comment_handler
!public :: xml_declaration_handler
!public :: cdata_section_handler
!public :: error_handler
!public :: signal_handler
!public :: empty_element_handler

end module FoX_sax
