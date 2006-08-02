module m_wcml_lattice

  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_EndElement
  use FoX_wxml, only: xml_AddAttribute, xml_AddCharacters
  use m_wcml_stml, only: stmAddValue

  implicit none
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)


  character(len=*), parameter :: U_ANGSTR = 'units:angstrom'
  character(len=*), parameter :: U_DEGREE = 'units:degree'
  character(len=*), parameter :: U_RADIAN = 'units:radian'

  interface cmlAddCrystal
     module procedure cmlAddCrystalSP
     module procedure cmlAddCrystalDP
  end interface

  interface cmlAddLattice
    module procedure cmlAddLatticeSP
    module procedure cmlAddLatticeDP
  end interface

  public :: cmlAddCrystal
  public :: cmlAddLattice

contains

  subroutine cmlAddCrystalSP(xf, a, b, c, alpha, beta, gamma, id, title, dictref, conv, lenunits, angunits, spaceType, fmt)
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)               :: a, b, c      ! cell parameters
    real(kind=sp), intent(in)               :: alpha        ! alpha cell parameter
    real(kind=sp), intent(in)               :: beta         ! beta cell parameter
    real(kind=sp), intent(in)               :: gamma        ! gamma cell parameter
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! convention
    character(len=*), intent(in), optional :: lenunits     ! units for length (default = angstrom)
    character(len=*), intent(in), optional :: angunits     ! units for angles (default = degree)
    character(len=*), intent(in), optional :: spaceType    ! spacegroup
    character(len=*), intent(in), optional :: fmt          ! format

    call xml_NewElement(xf=xf, name='crystal')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lenunits)) then
      call stmAddValue(xf=xf, value=a, title='a', dictref='cml:a', units=lenunits, fmt=fmt)
      call stmAddValue(xf=xf, value=b, title='b', dictref='cml:b', units=lenunits, fmt=fmt)
      call stmAddValue(xf=xf, value=c, title='c', dictref='cml:c', units=lenunits, fmt=fmt)
    else
      call stmAddValue(xf=xf, value=a, title='a', dictref='cml:a', units=U_ANGSTR, fmt=fmt)
      call stmAddValue(xf=xf, value=b, title='b', dictref='cml:b', units=U_ANGSTR, fmt=fmt)
      call stmAddValue(xf=xf, value=c, title='c', dictref='cml:c', units=U_ANGSTR, fmt=fmt)
    endif
    if (present(angunits)) then
      call stmAddValue(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=angunits, fmt=fmt)
      call stmAddValue(xf=xf, value=beta,  title='beta',  dictref='cml:beta',  units=angunits, fmt=fmt)
      call stmAddValue(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=angunits, fmt=fmt)
    else
      call stmAddValue(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=U_DEGREE, fmt=fmt)
      call stmAddValue(xf=xf, value=beta,  title='beta',  dictref='cml:beta',  units=U_DEGREE, fmt=fmt)
      call stmAddValue(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=U_DEGREE, fmt=fmt)
    endif
    if (present(spaceType)) then
      call xml_NewElement(xf, 'symmetry')
      call xml_AddAttribute(xf, 'spaceGroup', spaceType)
      call xml_EndElement(xf, 'symmetry')
    endif
    call xml_EndElement(xf, 'crystal')

  end subroutine cmlAddCrystalSP


  subroutine cmlAddCrystalDP(xf, a, b, c, alpha, beta, gamma, id, title, dictref, conv, lenunits, angunits, spaceType, fmt)
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), intent(in)               :: a, b, c      ! cell parameters
    real(kind=dp), intent(in)               :: alpha        ! alpha cell parameter
    real(kind=dp), intent(in)               :: beta         ! beta cell parameter
    real(kind=dp), intent(in)               :: gamma        ! gamma cell parameter
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! convention
    character(len=*), intent(in), optional :: lenunits     ! units for length (default = angstrom)
    character(len=*), intent(in), optional :: angunits     ! units for angles (default = degree)
    character(len=*), intent(in), optional :: spaceType    ! spacegroup
    character(len=*), intent(in), optional :: fmt          ! format

    call xml_NewElement(xf=xf, name='crystal')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lenunits)) then
      call stmAddValue(xf=xf, value=a, title='a', dictref='cml:a', units=lenunits, fmt=fmt)
      call stmAddValue(xf=xf, value=b, title='b', dictref='cml:b', units=lenunits, fmt=fmt)
      call stmAddValue(xf=xf, value=c, title='c', dictref='cml:c', units=lenunits, fmt=fmt)
    else
      call stmAddValue(xf=xf, value=a, title='a', dictref='cml:a', units=U_ANGSTR, fmt=fmt)
      call stmAddValue(xf=xf, value=b, title='b', dictref='cml:b', units=U_ANGSTR, fmt=fmt)
      call stmAddValue(xf=xf, value=c, title='c', dictref='cml:c', units=U_ANGSTR, fmt=fmt)
    endif
    if (present(angunits)) then
      call stmAddValue(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=angunits, fmt=fmt)
      call stmAddValue(xf=xf, value=beta,  title='beta',  dictref='cml:beta',  units=angunits, fmt=fmt)
      call stmAddValue(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=angunits, fmt=fmt)
    else
      call stmAddValue(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=U_DEGREE, fmt=fmt)
      call stmAddValue(xf=xf, value=beta,  title='beta',  dictref='cml:beta',  units=U_DEGREE, fmt=fmt)
      call stmAddValue(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=U_DEGREE, fmt=fmt)
    endif
    if (present(spaceType)) then
      call xml_NewElement(xf, 'symmetry')
      call xml_AddAttribute(xf, 'spaceGroup', spaceType)
      call xml_EndElement(xf, 'symmetry')
    endif
    call xml_EndElement(xf, 'crystal')

  end subroutine cmlAddCrystalDP


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



