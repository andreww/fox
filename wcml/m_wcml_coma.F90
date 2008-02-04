module m_wcml_coma
  ! Implements routines relating to electronic structure

  use m_common_realtypes, only: sp, dp
  use FoX_wxml, only: xmlf_t
#ifndef DUMMYLIB
  use FoX_common, only: str
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement, xml_AddCharacters
  use m_wcml_stml, only: stmAddValue

! Fix for pgi, requires this explicitly:
  use m_wxml_overloads
#endif

  implicit none
  private

  public :: cmlStartKpoint
  public :: cmlEndKpoint
  public :: cmlAddKpoint

  public :: cmlStartBand
  public :: cmlEndBand

  public :: cmlAddEigenValue
  public :: cmlAddEigenValueVector

  interface cmlAddEigenValue
    module procedure cmlAddEigenValueSP
    module procedure cmlAddEigenValueDP
  end interface

  interface cmlAddEigenValueVector
    module procedure cmlAddEigenValueVectorSP
    module procedure cmlAddEigenValueVectorDP
    module procedure cmlAddEigenValueVectorCmplxSP
    module procedure cmlAddEigenValueVectorCmplxDP
  end interface

  interface cmlStartKpoint
    module procedure cmlStartKpoint_sp
    module procedure cmlStartKpoint_dp
  end interface

  interface cmlAddKpoint
    module procedure cmlAddKpoint_sp
    module procedure cmlAddKpoint_dp
  end interface

contains


  subroutine cmlStartKPoint_sp(xf, kpoint, weight, kptfmt, wtfmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)              :: xf
    real(kind=sp), dimension(3), intent(in)  :: kpoint
    real(kind=sp), intent(in), optional      :: weight
    character(len=*), intent(in), optional   :: kptfmt
    character(len=*), intent(in), optional   :: wtfmt
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef DUMMYLIB
    call xml_NewElement(xf, "kpoint")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(ref)) call xml_addAttribute(xf, "ref", ref)
    if (present(label)) call xml_addAttribute(xf, "label", label)


    if (present(weight)) then
      if (present(wtfmt)) then
        call xml_AddAttribute(xf, "weight", str(weight, wtfmt))
      else
        call xml_AddAttribute(xf, "weight", weight)
      endif
    endif

    if (present(kptfmt)) then
      call xml_AddCharacters(xf, kpoint, kptfmt)
    else
      call xml_AddCharacters(xf, kpoint)
    end if

    call xml_EndElement(xf, "kpoint")
#endif

  end subroutine cmlStartKPoint_sp

  subroutine cmlAddKPoint_sp(xf, kpoint, weight &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=sp), dimension(3), intent(in) :: kpoint
    real(kind=sp), intent(in), optional     :: weight
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef DUMMYLIB
    call cmlStartKpoint(xf, kpoint, weight &
,dictRef,convention,title,id,ref,label)
    call cmlEndKpoint(xf)
#endif

  end subroutine cmlAddKPoint_sp

  subroutine cmlAddEigenValuesp(xf, value, units, fmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: value
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: fmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef DUMMYLIB
    call xml_NewElement(xf, "eigen")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=value, fmt=fmt, units=units)
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValuesp

  subroutine cmlAddEigenValueVectorsp(xf, eigval, eigvec, units, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: eigval(:)
    real(kind=sp), intent(in)              :: eigvec(:,:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef DUMMYLIB
    call xml_NewElement(xf, "eigen")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=eigval, fmt=valfmt, units=units)
    call stmAddValue(xf=xf, value=eigvec, fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValueVectorsp

  subroutine cmlAddEigenValueVectorCmplxsp(xf, eigval, eigvec, units, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: eigval(:)
    complex(kind=sp), intent(in)           :: eigvec(:,:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef DUMMYLIB
    call xml_NewElement(xf, "eigen")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=eigval, fmt=valfmt, units=units)
    call stmAddValue(xf=xf, value=eigvec, fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValueVectorCmplxsp



  subroutine cmlStartKPoint_dp(xf, kpoint, weight, kptfmt, wtfmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)              :: xf
    real(kind=dp), dimension(3), intent(in)  :: kpoint
    real(kind=dp), intent(in), optional      :: weight
    character(len=*), intent(in), optional   :: kptfmt
    character(len=*), intent(in), optional   :: wtfmt
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef DUMMYLIB
    call xml_NewElement(xf, "kpoint")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(ref)) call xml_addAttribute(xf, "ref", ref)
    if (present(label)) call xml_addAttribute(xf, "label", label)


    if (present(weight)) then
      if (present(wtfmt)) then
        call xml_AddAttribute(xf, "weight", str(weight, wtfmt))
      else
        call xml_AddAttribute(xf, "weight", weight)
      endif
    endif

    if (present(kptfmt)) then
      call xml_AddCharacters(xf, kpoint, kptfmt)
    else
      call xml_AddCharacters(xf, kpoint)
    end if

    call xml_EndElement(xf, "kpoint")
#endif

  end subroutine cmlStartKPoint_dp

  subroutine cmlAddKPoint_dp(xf, kpoint, weight &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=dp), dimension(3), intent(in) :: kpoint
    real(kind=dp), intent(in), optional     :: weight
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef DUMMYLIB
    call cmlStartKpoint(xf, kpoint, weight &
,dictRef,convention,title,id,ref,label)
    call cmlEndKpoint(xf)
#endif

  end subroutine cmlAddKPoint_dp

  subroutine cmlAddEigenValuedp(xf, value, units, fmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: value
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: fmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef DUMMYLIB
    call xml_NewElement(xf, "eigen")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=value, fmt=fmt, units=units)
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValuedp

  subroutine cmlAddEigenValueVectordp(xf, eigval, eigvec, units, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: eigval(:)
    real(kind=dp), intent(in)              :: eigvec(:,:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef DUMMYLIB
    call xml_NewElement(xf, "eigen")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=eigval, fmt=valfmt, units=units)
    call stmAddValue(xf=xf, value=eigvec, fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValueVectordp

  subroutine cmlAddEigenValueVectorCmplxdp(xf, eigval, eigvec, units, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: eigval(:)
    complex(kind=dp), intent(in)           :: eigvec(:,:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef DUMMYLIB
    call xml_NewElement(xf, "eigen")
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)



    call stmAddValue(xf=xf, value=eigval, fmt=valfmt, units=units)
    call stmAddValue(xf=xf, value=eigvec, fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValueVectorCmplxdp



  subroutine cmlStartBand(xf, spin &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in), optional :: spin
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef DUMMYLIB
    call xml_NewElement(xf, "band")
    if (spin=="up".or.spin=="down") then
      call xml_AddAttribute(xf, "spin", spin)
    else
      !error
    endif
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(ref)) call xml_addAttribute(xf, "ref", ref)
    if (present(label)) call xml_addAttribute(xf, "label", label)


#endif

  end subroutine cmlStartBand

  subroutine cmlEndKpoint(xf)
    type(xmlf_t), intent(inout) :: xf
#ifndef DUMMYLIB
    call xml_EndElement(xf, "kpoint")
#endif
  end subroutine cmlEndKpoint

  subroutine cmlEndBand(xf)
    type(xmlf_t), intent(inout) :: xf
#ifndef DUMMYLIB
    call xml_EndElement(xf, "band")
#endif
  end subroutine cmlEndBand




end module m_wcml_coma
