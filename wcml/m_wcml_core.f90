module m_wcml_core

  use FoX_common, only: FoX_version
  use m_common_error, only: FoX_error
  use m_common_format, only: str
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_AddCharacters, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement, xml_AddNamespace
  use m_wcml_stml, only: stmAddValue

  implicit none
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)
  character(len=*), parameter :: spformat = '(es11.3)'
  character(len=*), parameter :: dpformat = '(es16.8)'
  
! CMLUnits
  character(len=*), parameter :: U_ANGSTR = 'units:angstrom'
  character(len=*), parameter :: U_DEGREE = 'units:degree'
  character(len=*), parameter :: U_RADIAN = 'units:radian'

  public :: cmlStartCml
  public :: cmlEndCml

! CMLCore
  public :: cmlAddCrystal
  public :: cmlAddEigenvalue
  public :: cmlStartModule
  public :: cmlEndModule

  public :: cmlStartStep
  public :: cmlEndStep

  interface cmlAddCrystal
     module procedure cmlAddCrystalSP
     module procedure cmlAddCrystalDP
  end interface

  interface cmlAddEigenvalue
     module procedure cmlAddEigenvalueSP
     module procedure cmlAddEigenvalueDP
  end interface

contains

  ! =================================================
  ! convenience CML routines
  ! =================================================

! Open and close a CML document.

  subroutine cmlStartCml(xf, id, title, conv, dictref, ref)
    type(xmlf_t), intent(inout) :: xf

    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref

    call xml_AddNamespace(xf, 'http://www.xml-cml.org/schema')
    call xml_AddNamespace(xf, 'http://www.w3.org/2001/XMLSchema', 'xsd')
    call xml_AddNamespace(xf, 'http://purl.org/dc/elements/1.1/title', 'dc')
! FIXME TOHW we will want other namespaces in here - particularly for units
! once PMR has stabilized that.

    call xml_NewElement(xf, 'cml')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)

  end subroutine cmlStartCml

  subroutine cmlEndCml(xf)
    type(xmlf_t), intent(inout) :: xf

  !  call cmlAddMetadata(xf, name='dc:contributor', content='FoX-'//FoX_version//' (http://www.eminerals.org)')
    call xml_EndElement(xf, 'cml')

  end subroutine cmlEndCml

  ! -------------------------------------------------
  ! writes a Module start/end Tag to xml channel
  ! -------------------------------------------------
  subroutine cmlStartModule(xf, id, title, conv, dictref, ref, role, serial)

    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role
    character(len=*), intent(in), optional :: serial
    
    call xml_NewElement(xf, 'module')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    if (present(serial)) call xml_AddAttribute(xf, 'serial', serial)
    
  end subroutine cmlStartModule

  subroutine cmlEndModule(xf)

    type(xmlf_t), intent(inout) :: xf

    Call xml_EndElement(xf, 'module')
    
  end subroutine cmlEndModule

  ! -------------------------------------------------
  ! writes a step start Tag to xml channel
  ! -------------------------------------------------

  subroutine cmlStartStep(xf, type, index, id, title, conv, ref)

    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: type
    character(len=*), intent(in), optional :: id
    integer, intent(in), optional :: index
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref

    if (present(index)) then
      call cmlStartModule(xf=xf, id=id, title=title, conv=conv, dictRef=type, ref=ref, role='step', serial=str(index))
    else
      call cmlStartModule(xf=xf, id=id, title=title, conv=conv, dictRef=type, ref=ref, role='step')
    endif
    
  end subroutine cmlStartStep

  subroutine cmlEndStep(xf)

    type(xmlf_t), intent(inout) :: xf

    Call xml_EndElement(xf, 'module')
    
  end subroutine cmlEndStep



  ! -------------------------------------------------
  ! 1. creates and writes a DP <cell> element
  ! -------------------------------------------------

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

    ! Flush on entry and exit
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = dpformat
    endif

    call xml_NewElement(xf=xf, name='crystal')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lenunits)) then
      call stmAddValue(xf=xf, value=a, title='a', dictref='cml:a', units=lenunits, fmt=formt)
      call stmAddValue(xf=xf, value=b, title='b', dictref='cml:b', units=lenunits, fmt=formt)
      call stmAddValue(xf=xf, value=c, title='c', dictref='cml:c', units=lenunits, fmt=formt)
    else
      call stmAddValue(xf=xf, value=a, title='a', dictref='cml:a', units=U_ANGSTR, fmt=formt)
      call stmAddValue(xf=xf, value=b, title='b', dictref='cml:b', units=U_ANGSTR, fmt=formt)
      call stmAddValue(xf=xf, value=c, title='c', dictref='cml:c', units=U_ANGSTR, fmt=formt)
    endif
    if (present(angunits)) then
      call stmAddValue(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=angunits, fmt=formt)
      call stmAddValue(xf=xf, value=beta,  title='beta',  dictref='cml:beta',  units=angunits, fmt=formt)
      call stmAddValue(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=angunits, fmt=formt)
    else
      call stmAddValue(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=U_DEGREE, fmt=formt)
      call stmAddValue(xf=xf, value=beta,  title='beta',  dictref='cml:beta',  units=U_DEGREE, fmt=formt)
      call stmAddValue(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=U_DEGREE, fmt=formt)
    endif
    if (present(spaceType)) then
      call xml_NewElement(xf, 'symmetry')
      call xml_AddAttribute(xf, 'spaceGroup', spaceType)
      call xml_EndElement(xf, 'symmetry')
    endif
    call xml_EndElement(xf, 'crystal')

  end subroutine cmlAddCrystalDP

  ! -------------------------------------------------
  ! 2. creates and writes a SP <cell> element
  ! -------------------------------------------------

  subroutine cmlAddCrystalSP(xf, a, b, c, alpha, beta, gamma, id, title, dictref, conv, lenunits, angunits, spaceType, fmt)
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)     :: a, b, c      ! cell parameters
    real(kind=sp), intent(in)     :: alpha        ! alpha cell parameter
    real(kind=sp), intent(in)     :: beta         ! beta cell parameter
    real(kind=sp), intent(in)     :: gamma        ! gamma cell parameter
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! convention
    character(len=*), intent(in), optional :: lenunits     ! units for length (' ' = angstrom)
    character(len=*), intent(in), optional :: angunits     ! units for angles (' ' = degree)
    character(len=*), intent(in), optional :: spaceType    ! spacegroup
    character(len=*), intent(in), optional :: fmt          ! format

    ! Flush on entry and exit
    character(len=30) :: lunits, aunits
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = spformat
    endif
    if (present(lenunits)) then
       lunits = lenunits
    else
       lunits = U_ANGSTR
    endif
    if (present(angunits)) then
       aunits = angunits
    else
       aunits = U_DEGREE
    endif

    call xml_NewElement(xf=xf, name='crystal')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    call stmAddValue(xf=xf, value=a, title='a', dictref='cml:a', units=lunits, fmt=formt)
    call stmAddValue(xf=xf, value=b, title='b', dictref='cml:b', units=lunits, fmt=formt)
    call stmAddValue(xf=xf, value=c, title='c', dictref='cml:c', units=lunits, fmt=formt)
    call stmAddValue(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=aunits, fmt=formt)
    call stmAddValue(xf=xf, value=beta,  title='beta',  dictref='cml:beta', units=aunits, fmt=formt)
    call stmAddValue(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=aunits, fmt=formt)
    if (present(spaceType)) then
      call xml_NewElement(xf, 'symmetry')
      call xml_AddAttribute(xf, 'spaceGroup', spaceType)
      call xml_EndElement(xf, 'symmetry')
    endif
    call xml_EndElement(xf, 'crystal')


  end subroutine cmlAddCrystalSP
  
  
  ! -------------------------------------------------
  ! 1. creates and writes an DP <eigen> element
  ! -------------------------------------------------
  
  subroutine cmlAddEigenvalueDP(xf, n, eigvec, eigval, id, title, dictref, fmt)


    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: n              ! number of elements
    real(kind=dp), intent(in)              :: eigvec(n, *)   ! eigenvectors
    real(kind=dp), intent(in)              :: eigval(*)      ! eigenvalues
    character(len=*), intent(in), optional :: id             ! id
    character(len=*), intent(in), optional :: title          ! title
    character(len=*), intent(in), optional :: dictref        ! dictionary reference
    character(len=*), intent(in), optional :: fmt            ! format
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = dpformat
    endif

    ! Flush on entry and exit
    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    call stmAddValue(xf=xf, value=eigval(1:n), title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec(:n,:n), title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')

  end subroutine cmlAddEigenvalueDP



  ! -------------------------------------------------
  ! 2. creates and writes an SP <eigen> element
  ! -------------------------------------------------
  
  subroutine cmlAddEigenvalueSP(xf, n, eigvec, eigval, id, title, dictref, fmt)


    type(xmlf_t), intent(inout) :: xf
    integer, intent(in)          :: n              ! number of elements
    real(kind=sp), intent(in)     :: eigvec(n, *) ! eigenvectors
    real(kind=sp), intent(in)     :: eigval(*)      ! eigenvalues
    character(len=*), intent(in), optional :: id             ! id
    character(len=*), intent(in), optional :: title          ! title
    character(len=*), intent(in), optional :: dictref        ! dictionary reference
    character(len=*), intent(in), optional :: fmt            ! format
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = spformat
    endif

    ! Flush on entry and exit
    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictRef))   call xml_AddAttribute(xf, 'dictRef', dictref)
    call stmAddValue(xf=xf, value=eigval(1:n), title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec(:n,:n), title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')

  end subroutine cmlAddEigenvalueSP

  
end module m_wcml_core
