module m_dom_entity

  implicit none
  private
  
  public :: dom_entity

  type dom_entity
     character, dimension(:) :: notationName
     character, dimension(:) :: publicId
     character, dimension(:) :: systemId
  end type dom_entity

end module m_dom_entity
