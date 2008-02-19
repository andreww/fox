module m_wcml_coma
  ! Implements routines relating to electronic structure

  use m_common_realtypes, only: sp, dp
  use FoX_wxml, only: xmlf_t
#ifndef DUMMYLIB
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement
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
  public :: cmlAddBandList
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
    module procedure cmlStartKpointSP
    module procedure cmlStartKpointDP
  end interface

  interface cmlAddKpoint
    module procedure cmlAddKpointSP
    module procedure cmlAddKpointDP
  end interface

  interface cmlAddBandList
    module procedure cmlAddBandListSP
    module procedure cmlAddBandListDP
  end interface

contains


  subroutine cmlStartKPointsp(xf, coords, weight, kptfmt, wtfmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)              :: xf
    real(kind=sp), dimension(3), intent(in)  :: coords
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



    call xml_AddAttribute(xf, "coords", coords, kptfmt)
    if (present(weight)) &
       call xml_AddAttribute(xf, "weight", weight, wtfmt)
#endif

  end subroutine cmlStartKPointsp

  subroutine cmlAddKPointsp(xf, coords, weight, kptfmt, wtfmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=sp), dimension(3), intent(in) :: coords
    real(kind=sp), intent(in), optional     :: weight
    character(len=*), intent(in), optional   :: kptfmt
    character(len=*), intent(in), optional   :: wtfmt
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef DUMMYLIB
    call cmlStartKpoint(xf, coords, weight, kptfmt, wtfmt &
,dictRef,convention,title,id,ref,label)
    call cmlEndKpoint(xf)
#endif

  end subroutine cmlAddKPointsp

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

  subroutine cmlAddEigenValueVectorsp(xf, eigval, eigvec, units, valfmt, vecfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: eigval
    real(kind=sp), intent(in)              :: eigvec(:,:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: valfmt
    character(len=*), intent(in), optional :: vecfmt

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
!FIXME check 2nd dimension of matrix is 3
    call stmAddValue(xf=xf, value=eigvec, fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValueVectorsp

  subroutine cmlAddEigenValueVectorCmplxsp(xf, eigval, eigvec, units, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: eigval
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

  subroutine cmlAddBandListsp(xf, values, fmt, units, spin &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: values(:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: spin
    character(len=*), intent(in), optional :: fmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef DUMMYLIB
    call xml_NewElement(xf, "bandList")
    if (spin=="up".or.spin=="down") then
      call xml_AddAttribute(xf, "spin", spin)
    else
      !error
    endif
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)


    call xml_NewElement(xf, "eigen")
    call stmAddValue(xf=xf, value=values, fmt=fmt, units=units)
    call xml_EndElement(xf, "eigen")
    call xml_EndElement(xf, "bandList")
#endif

  end subroutine cmlAddBandListsp



  subroutine cmlStartKPointdp(xf, coords, weight, kptfmt, wtfmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)              :: xf
    real(kind=dp), dimension(3), intent(in)  :: coords
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



    call xml_AddAttribute(xf, "coords", coords, kptfmt)
    if (present(weight)) &
       call xml_AddAttribute(xf, "weight", weight, wtfmt)
#endif

  end subroutine cmlStartKPointdp

  subroutine cmlAddKPointdp(xf, coords, weight, kptfmt, wtfmt &
,dictRef,convention,title,id,ref,label)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=dp), dimension(3), intent(in) :: coords
    real(kind=dp), intent(in), optional     :: weight
    character(len=*), intent(in), optional   :: kptfmt
    character(len=*), intent(in), optional   :: wtfmt
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: label



#ifndef DUMMYLIB
    call cmlStartKpoint(xf, coords, weight, kptfmt, wtfmt &
,dictRef,convention,title,id,ref,label)
    call cmlEndKpoint(xf)
#endif

  end subroutine cmlAddKPointdp

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

  subroutine cmlAddEigenValueVectordp(xf, eigval, eigvec, units, valfmt, vecfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: eigval
    real(kind=dp), intent(in)              :: eigvec(:,:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: valfmt
    character(len=*), intent(in), optional :: vecfmt

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
!FIXME check 2nd dimension of matrix is 3
    call stmAddValue(xf=xf, value=eigvec, fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValueVectordp

  subroutine cmlAddEigenValueVectorCmplxdp(xf, eigval, eigvec, units, vecfmt, valfmt &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: eigval
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

  subroutine cmlAddBandListdp(xf, values, fmt, units, spin &
,dictRef,convention,title,id,type)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: values(:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: spin
    character(len=*), intent(in), optional :: fmt

    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: type



#ifndef DUMMYLIB
    call xml_NewElement(xf, "bandList")
    if (spin=="up".or.spin=="down") then
      call xml_AddAttribute(xf, "spin", spin)
    else
      !error
    endif
    if (present(dictRef)) call xml_addAttribute(xf, "dictRef", dictRef)
    if (present(convention)) call xml_addAttribute(xf, "convention", convention)
    if (present(title)) call xml_addAttribute(xf, "title", title)
    if (present(id)) call xml_addAttribute(xf, "id", id)
    if (present(type)) call xml_addAttribute(xf, "type", type)


    call xml_NewElement(xf, "eigen")
    call stmAddValue(xf=xf, value=values, fmt=fmt, units=units)
    call xml_EndElement(xf, "eigen")
    call xml_EndElement(xf, "bandList")
#endif

  end subroutine cmlAddBandListdp



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
