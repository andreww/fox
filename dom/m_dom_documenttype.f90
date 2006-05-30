module m_dom_documenttype
  
  use m_dom_namednodemap, only : fNamedNodeMap

  implicit none
  private

  public :: fDocumentType

  type fDocumentType
     character, dimension(:), pointer :: name
     type(fNamedNodeMap)              :: entities
     type(fNamedNodeMap)              :: notations
     character, dimension(:) :: publicId
     character, dimension(:) :: systemId
     character, dimension(:) :: internalSubset
  end type fDocumentType

end module m_dom_documenttype
