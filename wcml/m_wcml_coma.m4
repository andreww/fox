dnl
dnl First part is boilerplate to give us a foreach function
dnl
divert(-1)
# foreach(x, (item_1, item_2, ..., item_n), stmt)
define(`m4_foreach', `pushdef(`$1', `')_foreach(`$1', `$2', `$3')popdef(`$1')')
define(`_arg1', `$1')
define(`_foreach',
        `ifelse(`$2', `()', ,
                `define(`$1', _arg1$2)$3`'_foreach(`$1', (shift$2), `$3')')')
# traceon(`define', `foreach', `_foreach', `ifelse')
divert 
dnl given a variable name a, declare it as follows:
define(`TOHWM4_dummyargdecl',`dnl
    character(len=*), intent(in), optional :: $1
')dnl
dnl
dnl use an optional character variable:
define(`TOHWM4_dummyarguse',`dnl
    if (present($1)) call xml_addAttribute(xf, "$1", $1)
')dnl
dnl Below we only use arguments with a type of xsd:string
define(`TOHWM4_bandargs', `(dictRef,convention,title,id,ref,label)')dnl
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
dnl Below we only use arguments with a type of xsd:string
define(`TOHWM4_eigenargs', `(dictRef,convention,title,id,type)')dnl
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
define(`TOHWM4_coma_subs', `dnl
dnl subroutine cmlAddBand_kpt_$1(xf, kpoint, kweight, units, bands, kptfmt, eigfmt)
dnl     type(xmlf_t), intent(inout)            :: xf
dnl     real($1), intent(in) :: kpoint(3)
dnl     real($1), intent(in) :: bands(:)
dnl     real($1), intent(in), optional :: kweight
dnl     character(len=*), intent(in) :: units
dnl     character(len=*), intent(in), optional :: kptfmt
dnl     character(len=*), intent(in), optional :: eigfmt
dnl 
dnl     call xml_NewElement(xf, "band")
dnl     if (present(kweight)) call xml_AddAttribute(xf, "weight", kweight)
dnl     if (present(kptfmt)) then
dnl       call xml_AddAttribute(xf, "kpoint", str(kpoint, kptfmt))
dnl     else
dnl       call xml_AddAttribute(xf, "kpoint", str(kpoint, "r7"))
dnl     endif
dnl 
dnl     call stmAddValue(xf, value=bands, fmt=eigfmt, units=units)
dnl     call xml_EndElement(xf, "band")
dnl 
dnl   end subroutine cmlAddBand_kpt_$1

  subroutine cmlAddBand_kptref_$1(xf, kptref, bands, units, fmt &
TOHWM4_bandargslist)
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in) :: kptref
    real($1), intent(in) :: bands(:)
    character(len=*), intent(in) :: units
    character(len=*), intent(in), optional :: fmt
TOHWM4_bandargsdecl

    call xml_NewElement(xf, "band")
    call xml_AddAttribute(xf, "kpointRef", kptref)
TOHWM4_bandargsuse
    call stmAddValue(xf, value=bands, fmt=fmt, units=units)
    call xml_EndElement(xf, "band")

  end subroutine cmlAddBand_kptref_$1

  subroutine cmlAddKPoint_$1(xf, kpoint, weight, weightfmt, kptfmt &
TOHWM4_bandargslist)
    type(xmlf_t), intent(inout) :: xf
    real(kind=$1), dimension(3), intent(in)  :: kpoint
    real(kind=$1), intent(in), optional  :: weight
    character(len=*), intent(in), optional :: weightfmt
    character(len=*), intent(in), optional :: kptfmt
TOHWM4_bandargsdecl

    call xml_NewElement(xf, "kpoint")
TOHWM4_bandargsuse
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

  end subroutine cmlAddKPoint_$1

  subroutine cmlAddEigenvalue$1Sh(xf, eigvec, eigval, units, &
eigenOrientationType, vecfmt, valfmt &
TOHWM4_eigenargslist)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=$1), intent(in)              :: eigvec(:,:)
    real(kind=$1), intent(in)              :: eigval(:)
    character(len=*), intent(in)           :: eigenOrientationType
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

TOHWM4_eigenargsdecl

    call xml_NewElement(xf, "eigen")
    call xml_AddAttribute(xf, "eigenOrientationType", eigenOrientationType)
TOHWM4_eigenargsuse

    call stmAddValue(xf=xf, value=eigval, title="eigenvalues", dictref=dictRef, fmt=valfmt, units=units)
    call stmAddValue(xf=xf, value=eigvec, title="eigenvectors", fmt=vecfmt, units="units:dimensionless")
    call xml_EndElement(xf, "eigen")
    
  end subroutine cmlAddEigenvalue$1Sh

  subroutine cmlAddEigenvalue$1Si(xf, n, eigvec, eigval, units, &
eigenOrientationType, vecfmt, valfmt &
TOHWM4_eigenargslist)
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: n
    real(kind=$1), intent(in)              :: eigvec(n,*)
    real(kind=$1), intent(in)              :: eigval(*)
    character(len=*), intent(in)           :: eigenOrientationType
    character(len=*), intent(in)           :: units
    character(len=*), intent(in), optional :: vecfmt
    character(len=*), intent(in), optional :: valfmt

TOHWM4_eigenargsdecl

    call xml_NewElement(xf, "eigen")
    call xml_AddAttribute(xf, "eigenOrientationType", eigenOrientationType)
TOHWM4_eigenargsuse

    call stmAddValue(xf=xf, value=eigval(:n), title="eigenvalues", units=units, dictref=dictRef, fmt=valfmt)
    call stmAddValue(xf=xf, value=eigvec(:n,:n), title="eigenvectors", units="units:dimensionless", fmt=vecfmt)
    call xml_EndElement(xf, "eigen")
    
  end subroutine cmlAddEigenvalue$1Si
')dnl
dnl
module m_wcml_coma
  ! Implements routines relating to electronic structure

  use m_common_realtypes, only: sp, dp

  use FoX_common, only: str
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement, xml_AddCharacters
  use m_wcml_stml, only: stmAddValue

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
dnl    module procedure cmlAddBand_kpt_sp
    module procedure cmlAddBand_kptref_sp
dnl    module procedure cmlAddBand_kpt_dp
    module procedure cmlAddBand_kptref_dp
  end interface

  interface cmlAddKpoint
    module procedure cmlAddKpoint_sp
    module procedure cmlAddKpoint_dp
  end interface

contains

TOHWM4_coma_subs(`sp')

TOHWM4_coma_subs(`dp')

end module m_wcml_coma
