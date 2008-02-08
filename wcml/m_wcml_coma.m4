dnl
include(`foreach.m4')dnl
dnl
include(`common.m4')dnl
dnl
dnl Below we only use arguments with a type of xsd:string
define(`TOHWM4_bandargs', `(dictRef,convention,title,id,ref,label)')dnl
dnl
define(`TOHWM4_eigenargs', `(dictRef,convention,title,id,type)')dnl
dnl
define(`TOHWM4_bandargslist', `dnl
m4_foreach(`x', TOHWM4_bandargs, `,x')dnl
')dnl
define(`TOHWM4_bandargsdecl',`dnl
m4_foreach(`x',TOHWM4_bandargs,`TOHWM4_dummyargdecl(x)')
')dnl
define(`TOHWM4_bandargsuse',`dnl
m4_foreach(`x',TOHWM4_bandargs,`TOHWM4_dummyarguse(x)')
')dnl
dnl
define(`TOHWM4_eigenargslist', `dnl
m4_foreach(`x', TOHWM4_eigenargs, `,x')dnl
')dnl
define(`TOHWM4_eigenargsdecl',`dnl
m4_foreach(`x',TOHWM4_eigenargs,`TOHWM4_dummyargdecl(x)')
')dnl
define(`TOHWM4_eigenargsuse',`dnl
m4_foreach(`x',TOHWM4_eigenargs,`TOHWM4_dummyarguse(x)')
')dnl
dnl
define(`TOHWM4_coma_real_subs', `dnl

  subroutine cmlStartKPoint_$1(xf, kpoint, weight, kptfmt, wtfmt &
TOHWM4_bandargslist)
    type(xmlf_t), intent(inout)              :: xf
    real(kind=$1), dimension(3), intent(in)  :: kpoint
    real(kind=$1), intent(in), optional      :: weight
    character(len=*), intent(in), optional   :: kptfmt
    character(len=*), intent(in), optional   :: wtfmt
TOHWM4_bandargsdecl

#ifndef DUMMYLIB
    call xml_NewElement(xf, "kpoint")
TOHWM4_bandargsuse
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
#endif

  end subroutine cmlStartKPoint_$1

  subroutine cmlAddKPoint_$1(xf, kpoint, weight &
TOHWM4_bandargslist)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=$1), dimension(3), intent(in) :: kpoint
    real(kind=$1), intent(in), optional     :: weight
TOHWM4_bandargsdecl

#ifndef DUMMYLIB
    call cmlStartKpoint(xf, kpoint, weight &
TOHWM4_bandargslist)
    call cmlEndKpoint(xf)
#endif

  end subroutine cmlAddKPoint_$1

  subroutine cmlAddEigenValue$1(xf, value, units, fmt &
TOHWM4_eigenargslist)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=$1), intent(in)              :: value
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: fmt

TOHWM4_eigenargsdecl

#ifndef DUMMYLIB
    call xml_NewElement(xf, "eigen")
TOHWM4_eigenargsuse

    call stmAddValue(xf=xf, value=value, fmt=fmt, units=units)
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValue$1

  subroutine cmlAddEigenValueVector$1(xf, eigval, eigvec, units, vecfmt, valfmt &
TOHWM4_eigenargslist)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=$1), intent(in)              :: eigval(:)
    real(kind=$1), intent(in)              :: eigvec(:,:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

TOHWM4_eigenargsdecl

#ifndef DUMMYLIB
    call xml_NewElement(xf, "eigen")
TOHWM4_eigenargsuse

    call stmAddValue(xf=xf, value=eigval, fmt=valfmt, units=units)
    call stmAddValue(xf=xf, value=eigvec, fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValueVector$1

  subroutine cmlAddEigenValueVectorCmplx$1(xf, eigval, eigvec, units, vecfmt, valfmt &
TOHWM4_eigenargslist)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=$1), intent(in)              :: eigval(:)
    complex(kind=$1), intent(in)           :: eigvec(:,:)
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

TOHWM4_eigenargsdecl

#ifndef DUMMYLIB
    call xml_NewElement(xf, "eigen")
TOHWM4_eigenargsuse

    call stmAddValue(xf=xf, value=eigval, fmt=valfmt, units=units)
    call stmAddValue(xf=xf, value=eigvec, fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
#endif

  end subroutine cmlAddEigenValueVectorCmplx$1

')`'`'dnl
dnl
define(`TOHWM4_coma_subs', `dnl

  subroutine cmlStartBand(xf, spin &
TOHWM4_bandargslist)
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in), optional :: spin
TOHWM4_bandargsdecl

#ifndef DUMMYLIB
    call xml_NewElement(xf, "band")
    if (spin=="up".or.spin=="down") then
      call xml_AddAttribute(xf, "spin", spin)
    else
      !error
    endif
TOHWM4_bandargsuse
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

')`'`'dnl
dnl
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

TOHWM4_coma_real_subs(`sp')`'dnl

TOHWM4_coma_real_subs(`dp')`'dnl

TOHWM4_coma_subs


end module m_wcml_coma
