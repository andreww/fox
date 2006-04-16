program example
!
! Example driver for a stand-alone parsing of an xml document
!
use xmlf90_dom, only : fDocumentNode, parsefile, serialize, destroyDocument

implicit none

  type(fDocumentNode), pointer :: document

  document => parsefile("test.xml")

  call serialize(document, "out.xml")

  call destroyDocument()

end program example














