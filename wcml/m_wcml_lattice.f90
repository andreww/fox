module m_wcml_lattice

  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_EndElement
  use FoX_wxml, only: xml_AddAttribute, xml_AddCharacters

  implicit none
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)

  interface cmlAddLattice
    module procedure cmlAddLatticeSP
    module procedure cmlAddLatticeDP
  end interface

  public :: cmlAddLattice

contains

  subroutine cmlAddLatticeSP(xf, cell, units, title, id, dictref, conv, lattType, spaceType, fmt)

    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)              :: cell(3,3)
    character(len=*), intent(in), optional :: units       
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! convention
    character(len=*), intent(in), optional :: lattType 
    character(len=*), intent(in), optional :: spaceType    !
    character(len=*), intent(in), optional :: fmt         

    integer :: i

    call xml_NewElement(xf, 'lattice')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lattType)) call xml_AddAttribute(xf, 'latticeType', lattType)
    if (present(spaceType)) call xml_AddAttribute(xf, 'spaceType', spaceType)

    do i = 1,3
      call xml_NewElement(xf, 'latticeVector')
      if (present(units)) call xml_AddAttribute(xf, 'units', units)
      call xml_AddAttribute(xf, 'dictRef', 'cml:latticeVector')
      call xml_AddCharacters(xf, cell(:,i), fmt)
      call xml_EndElement(xf, 'latticeVector')
    enddo
    call xml_EndElement(xf, 'lattice')

  end subroutine cmlAddLatticeSP

  subroutine cmlAddLatticeDP(xf, cell, units, title, id, dictref, conv, lattType, spaceType, fmt)

    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), intent(in)              :: cell(3,3)
    character(len=*), intent(in), optional :: units       
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! 
    character(len=*), intent(in), optional :: lattType     ! 
    character(len=*), intent(in), optional :: spaceType    !
    character(len=*), intent(in), optional :: fmt         

    integer :: i

    call xml_NewElement(xf, 'lattice')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lattType)) call xml_AddAttribute(xf, 'latticeType', lattType)
    if (present(spaceType)) call xml_AddAttribute(xf, 'spaceType', spaceType)

    do i = 1,3
      call xml_NewElement(xf, 'latticeVector')
      if (present(units)) call xml_AddAttribute(xf, 'units', units)
      call xml_AddAttribute(xf, 'dictRef', 'cml:latticeVector')
      call xml_AddCharacters(xf, cell(:,i), fmt)
      call xml_EndElement(xf, 'latticeVector')
    enddo
    call xml_EndElement(xf, 'lattice')

  end subroutine cmlAddLatticeDP

end module m_wcml_lattice



