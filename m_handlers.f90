module m_handlers

use FoX_common
use FoX_sax

private

!
! A prototype of a specific language processor.
! It defines the routines that are called from xml_parser in response
! to particular events.
!
! In this particular example we just print the names of the elements
! and the content of the pcdata chunks, as well as any comments, XML
! and SGML declarations, etc.
!
! A module such as this could use "utility routines" to convert pcdata
! to numerical arrays, and to populate specific data structures.
!
public :: begin_element_handler, end_element_handler, pcdata_chunk_handler
public :: comment_handler, xml_declaration_handler, sgml_declaration_handler

CONTAINS  !=============================================================

subroutine begin_element_handler(URI, localname, name,attributes)
character(len=*), intent(in)   :: URI
character(len=*), intent(in)   :: localname
character(len=*), intent(in)   :: name
type(dictionary_t), intent(in) :: attributes

write(unit=*,fmt="(4a)") ">>Begin Element: {", URI, "}", localname
write(unit=*,fmt="(a,i2,a)") "--- ", len(attributes), " attributes:"
call print_dict(attributes)
end subroutine begin_element_handler

!--------------------------------------------------
subroutine end_element_handler(URI, localname, name)
character(len=*), intent(in)     :: URI
character(len=*), intent(in)     :: localname
character(len=*), intent(in)     :: name

write(unit=*,fmt="(4a)") ">>End Element: {", URI, "}", localname

end subroutine end_element_handler

!--------------------------------------------------
subroutine pcdata_chunk_handler(chunk)
character(len=*), intent(in) :: chunk

write(unit=*,fmt="(a)",advance="no") chunk

end subroutine pcdata_chunk_handler

!--------------------------------------------------
subroutine comment_handler(comment)
character(len=*), intent(in) :: comment

write(unit=*,fmt="(a)") ">>Comment: "
write(unit=*,fmt="(a)") comment

end subroutine comment_handler

!--------------------------------------------------
subroutine xml_declaration_handler(name,attributes)
character(len=*), intent(in)   :: name
type(dictionary_t), intent(in) :: attributes
!
! Same structure as an element tag
!
 write(unit=*,fmt="(2a)") ">>XML declaration: ", name
 call print_dict(attributes)

end subroutine xml_declaration_handler

!--------------------------------------------------
subroutine sgml_declaration_handler(sgmldecl)
character(len=*), intent(in) :: sgmldecl
!
write(unit=*,fmt="(a)") ">>SGML declaration: "
write(unit=*,fmt="(a)") sgmldecl

end subroutine sgml_declaration_handler
!--------------------------------------------------

end module m_handlers












