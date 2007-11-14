module m_wcml_coma
  ! Implements routines relating to electronic structure

#ifdef WCML_DUMMY
  integer, parameter :: sp = selected_real_kind(6,30)
  integer, parameter :: dp = selected_real_kind(14,100)
  type xmlf_t
    integer :: i
  end type xmlf_t
#else
  use m_common_realtypes, only: sp, dp

  use FoX_common, only: str
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement, xml_AddCharacters
  use m_wcml_stml, only: stmAddValue

! Fix for pgi, requires this explicitly:
  use m_wxml_overloads
#endif

  implicit none
  private

  public :: cmlAddEigen
  public :: cmlAddBand
  public :: cmlAddKpoint

  interface cmlAddEigen
    module procedure cmlAddEigenvalueSPSh
    module procedure cmlAddEigenvalueDPSh
    module procedure cmlAddEigenvalueSPSi
    module procedure cmlAddEigenvalueDPSi
  end interface

  interface cmlAddBand
    module procedure cmlAddBand_kptref_sp
    module procedure cmlAddBand_kptref_dp
  end interface

  interface cmlAddKpoint
    module procedure cmlAddKpoint_sp
    module procedure cmlAddKpoint_dp
  end interface

contains


  subroutine cmlAddBand_kptref_sp(xf, kptref, bands, units, fmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in) :: kptref
    real(sp), intent(in) :: bands(:)
    character(len=*), intent(in) :: units
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef WCML_DUMMY
    call xml_NewElement(xf, "band")
    call xml_AddAttribute(xf, "kpointRef", kptref)
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(ref)) call xml_addAttribute(xf, "ref", ref)
    if (present(label)) call xml_addAttribute(xf, "label", label)


    call stmAddValue(xf, value=bands, fmt=fmt, units=units)
    call xml_EndElement(xf, "band")
#endif

  end subroutine cmlAddBand_kptref_sp

  subroutine cmlAddKPoint_sp(xf, kpoint, weight, weightfmt, kptfmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), dimension(3), intent(in)  :: kpoint
    real(kind=sp), intent(in), optional  :: weight
    character(len=*), intent(in), optional :: weightfmt
    character(len=*), intent(in), optional :: kptfmt
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef WCML_DUMMY
    call xml_NewElement(xf, "kpoint")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(ref)) call xml_addAttribute(xf, "ref", ref)
    if (present(label)) call xml_addAttribute(xf, "label", label)


    if (present(weight)) then
      if (present(weightfmt)) then
        call xml_AddAttribute(xf, "weight", str(weight, weightfmt))
      else
        call xml_AddAttribute(xf, "weight", str(weight, "r3"))
      endif
    endif

    if (present(kptfmt)) then
      call xml_AddCharacters(xf, kpoint, kptfmt)
    else
      call xml_AddCharacters(xf, kpoint, "r3")
    end if

    call xml_EndElement(xf, "kpoint")
#endif

  end subroutine cmlAddKPoint_sp

  subroutine cmlAddEigenvaluespSh(xf, eigvec, eigval, units, &
eigenOrientationType, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: eigvec(:,:)
    real(kind=sp), intent(in)              :: eigval(:)
    character(len=*), intent(in)           :: eigenOrientationType
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef WCML_DUMMY
    call xml_NewElement(xf, "eigen")
    call xml_AddAttribute(xf, "eigenOrientationType", eigenOrientationType)
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=eigval, title="eigenvalues", dictref=dictRef, fmt=valfmt, units=units)
    call stmAddValue(xf=xf, value=eigvec, title="eigenvectors", fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenvaluespSh

  subroutine cmlAddEigenvaluespSi(xf, n, eigvec, eigval, units, &
eigenOrientationType, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: n
    real(kind=sp), intent(in)              :: eigvec(n,*)
    real(kind=sp), intent(in)              :: eigval(*)
    character(len=*), intent(in)           :: eigenOrientationType
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef WCML_DUMMY
    call xml_NewElement(xf, "eigen")
    call xml_AddAttribute(xf, "eigenOrientationType", eigenOrientationType)
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=eigval(:n), title="eigenvalues", units=units, dictref=dictRef, fmt=valfmt)
    call stmAddValue(xf=xf, value=eigvec(:n,:n), title="eigenvectors", units="units:dimensionless", fmt=vecfmt)
    call xml_EndElement(xf, "eigen")
#endif
    
  end subroutine cmlAddEigenvaluespSi



  subroutine cmlAddBand_kptref_dp(xf, kptref, bands, units, fmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in) :: kptref
    real(dp), intent(in) :: bands(:)
    character(len=*), intent(in) :: units
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef WCML_DUMMY
    call xml_NewElement(xf, "band")
    call xml_AddAttribute(xf, "kpointRef", kptref)
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(ref)) call xml_addAttribute(xf, "ref", ref)
    if (present(label)) call xml_addAttribute(xf, "label", label)


    call stmAddValue(xf, value=bands, fmt=fmt, units=units)
    call xml_EndElement(xf, "band")
#endif

  end subroutine cmlAddBand_kptref_dp

  subroutine cmlAddKPoint_dp(xf, kpoint, weight, weightfmt, kptfmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), dimension(3), intent(in)  :: kpoint
    real(kind=dp), intent(in), optional  :: weight
    character(len=*), intent(in), optional :: weightfmt
    character(len=*), intent(in), optional :: kptfmt
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef WCML_DUMMY
    call xml_NewElement(xf, "kpoint")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(ref)) call xml_addAttribute(xf, "ref", ref)
    if (present(label)) call xml_addAttribute(xf, "label", label)


    if (present(weight)) then
      if (present(weightfmt)) then
        call xml_AddAttribute(xf, "weight", str(weight, weightfmt))
      else
        call xml_AddAttribute(xf, "weight", str(weight, "r3"))
      endif
    endif

    if (present(kptfmt)) then
      call xml_AddCharacters(xf, kpoint, kptfmt)
    else
      call xml_AddCharacters(xf, kpoint, "r3")
    end if

    call xml_EndElement(xf, "kpoint")
#endif

  end subroutine cmlAddKPoint_dp

  subroutine cmlAddEigenvaluedpSh(xf, eigvec, eigval, units, &
eigenOrientationType, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: eigvec(:,:)
    real(kind=dp), intent(in)              :: eigval(:)
    character(len=*), intent(in)           :: eigenOrientationType
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef WCML_DUMMY
    call xml_NewElement(xf, "eigen")
    call xml_AddAttribute(xf, "eigenOrientationType", eigenOrientationType)
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=eigval, title="eigenvalues", dictref=dictRef, fmt=valfmt, units=units)
    call stmAddValue(xf=xf, value=eigvec, title="eigenvectors", fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenvaluedpSh

  subroutine cmlAddEigenvaluedpSi(xf, n, eigvec, eigval, units, &
eigenOrientationType, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: n
    real(kind=dp), intent(in)              :: eigvec(n,*)
    real(kind=dp), intent(in)              :: eigval(*)
    character(len=*), intent(in)           :: eigenOrientationType
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef WCML_DUMMY
    call xml_NewElement(xf, "eigen")
    call xml_AddAttribute(xf, "eigenOrientationType", eigenOrientationType)
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=eigval(:n), title="eigenvalues", units=units, dictref=dictRef, fmt=valfmt)
    call stmAddValue(xf=xf, value=eigvec(:n,:n), title="eigenvectors", units="units:dimensionless", fmt=vecfmt)
    call xml_EndElement(xf, "eigen")
#endif
    
  end subroutine cmlAddEigenvaluedpSi


end module m_wcml_coma
