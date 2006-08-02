module m_wcml_coma
  ! Implements routines relating to electronic structure

  use m_common_format, only: str
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement, xml_AddCharacters
  use m_wcml_stml, only: stmAddValue

  implicit none
  private

  integer, private, parameter ::  sp = selected_real_kind(6,30)
  integer, private, parameter ::  dp = selected_real_kind(14,100)

  public :: cmlAddBand
  public :: cmlStartBandList
  public :: cmlEndBandList

  public :: cmlAddKPoint
  public :: cmlStartKPointList
  public :: cmlEndKPointList

  public :: cmlAddEigenvalue

  interface cmlAddEigenvalue
    module procedure cmlAddEigenvalueSPSh
    module procedure cmlAddEigenvalueDPSh
    module procedure cmlAddEigenvalueSPSi
    module procedure cmlAddEigenvalueDPSi
  end interface

  interface cmlAddBand
    module procedure cmlAddBand_kpt
    module procedure cmlAddBand_kptref
  end interface

contains

  subroutine cmlAddBand_kpt(xf, kpoint, kweight, bands, kptfmt, eigfmt)
    type(xmlf_t), intent(inout)            :: xf
    real(dp), intent(in) :: kpoint(3)
    real(dp), intent(in) :: kweight
    real(dp), intent(in) :: bands(:)
    character(len=*), intent(in), optional :: kptfmt
    character(len=*), intent(in), optional :: eigfmt

    call xml_NewElement(xf, 'band')
    call xml_AddAttribute(xf, 'weight', kweight)
    if (present(kptfmt)) then
      call xml_AddAttribute(xf, 'kpoint', str(kpoint, kptfmt))
    else
      call xml_AddAttribute(xf, 'kpoint', str(kpoint, "r7"))
    endif

    call stmAddValue(xf, value=bands, fmt=eigfmt)
    call xml_EndElement(xf, 'band')

  end subroutine cmlAddBand_kpt


  subroutine cmlAddBand_kptref(xf, kptref, bands, eigfmt)
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in) :: kptref
    real(dp), intent(in) :: bands(:)
    character(len=*), intent(in), optional :: eigfmt

    call xml_NewElement(xf, 'band')
    call xml_AddAttribute(xf, 'kpointRef', kptref)
    call stmAddValue(xf, value=bands, fmt=eigfmt)
    call xml_EndElement(xf, 'band')

  end subroutine cmlAddBand_kptref


  subroutine cmlStartBandList(xf, id, title, conv, dictref, ref, role)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role

    call xml_NewElement(xf, 'bandList')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)

  end subroutine cmlStartBandList


  subroutine cmlEndBandList(xf)
    type(xmlf_t), intent(inout) :: xf

    call xml_EndElement(xf, 'bandList')

  end subroutine cmlEndBandList


  subroutine cmlAddKPoint(xf, kpoint, weight, id, title, conv, dictref, weightfmt, kptfmt)
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), dimension(3), intent(in)  :: kpoint
    real(kind=dp), intent(in)  :: weight
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: weightfmt
    character(len=*), intent(in), optional :: kptfmt

    call xml_NewElement(xf, 'kpoint')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)

    if (present(weightfmt)) then
      call xml_AddAttribute(xf, 'weight', str(weight, weightfmt))
    else
      call xml_AddAttribute(xf, 'weight', str(weight, "r7"))
    endif

    if (present(kptfmt)) then
      call xml_AddCharacters(xf, kpoint, kptfmt)
    else
      call xml_AddCharacters(xf, kpoint, "r6")
    end if

    call xml_EndElement(xf, 'kpoint')

  end subroutine cmlAddKPoint


  subroutine cmlStartKPointList(xf, id, title, conv, dictref, ref, role)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role

    call xml_NewElement(xf, 'kpointList')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)

  end subroutine cmlStartKPointList


  subroutine cmlEndKPointList(xf)
    type(xmlf_t), intent(inout) :: xf

    call xml_EndElement(xf, 'kpointList')

  end subroutine cmlEndKPointList


  subroutine cmlAddEigenvalueDPSh(xf, eigvec, eigval, id, title, dictref, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: eigvec(:,:)
    real(kind=dp), intent(in)              :: eigval(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    call stmAddValue(xf=xf, value=eigval, title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec, title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')
    
  end subroutine cmlAddEigenvalueDPSh


  subroutine cmlAddEigenvalueDPSi(xf, n, eigvec, eigval, id, title, dictref, fmt)
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: n
    real(kind=dp), intent(in)              :: eigvec(n,*)
    real(kind=dp), intent(in)              :: eigval(*)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    call stmAddValue(xf=xf, value=eigval(:n), title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec(:n,:n), title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')
    
  end subroutine cmlAddEigenvalueDPSi


  subroutine cmlAddEigenvalueSPSh(xf, eigvec, eigval, id, title, dictref, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: eigvec(:,:)
    real(kind=sp), intent(in)              :: eigval(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    call stmAddValue(xf=xf, value=eigval, title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec, title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')
    
  end subroutine cmlAddEigenvalueSPSh


  subroutine cmlAddEigenvalueSPSi(xf, n, eigvec, eigval, id, title, dictref, fmt)
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: n
    real(kind=sp), intent(in)              :: eigvec(n,*)
    real(kind=sp), intent(in)              :: eigval(*)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    call stmAddValue(xf=xf, value=eigval(:n), title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddValue(xf=xf, value=eigvec(:n,:n), title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')
    
  end subroutine cmlAddEigenvalueSPSi

end module m_wcml_coma
