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
dnl
dnl Define a few basic bits
dnl
dnl
define(`TOHWM4_declarationtype', `dnl
ifelse(`$1', `RealDp', `real(dp)', 
       `$1', `RealSp', `real(sp)', 
       `$1', `Int', `integer', 
       `$1', `Lg', `logical', 
       `$1', `Ch', `character(len=*)') dnl
')dnl
dnl
dnl
define(`TOHWM4_subroutinename', `$1$2$3$4')dnl
define(`TOHWM4_interfacename',`module procedure TOHWM4_subroutinename(`$1',`$2',`$3',`$4')')dnl
dnl
define(`TOHWM4_interfacelist', `dnl
     TOHWM4_interfacename($1,`Sca',`$2',`')
     TOHWM4_interfacename($1,`Arr',`$2',`Si')
     TOHWM4_interfacename($1,`Arr',`$2',`Sh')
     TOHWM4_interfacename($1,`Mat',`$2',`Si')
     TOHWM4_interfacename($1,`Mat',`$2',`Sh')
')dnl
dnl
dnl
dnl given a list (a, b, c) strip off the brackets:
define(`TOHWM4_dummyarglist',`dnl
substr($1,1,decr(decr(len($1))))`'dnl
')dnl
dnl
dnl given a variable name a, declare it as follows:
define(`TOHWM4_dummyargdecl',`dnl
    character(len=*), intent(in), optional :: $1
')dnl
dnl
dnl use an optional character variable:
define(`TOHWM4_dummyarguse',`dnl
    if (present($1)) call xml_addAttribute(xf, "$1", $1)
')dnl
dnl
dnl
dnl Start/End QuantityLists.
dnl First argument is name of quantity,
dnl second is list of optional args.
define(`TOHWM4_QuantityList',`dnl
  subroutine cmlStart$1List(xf, TOHWM4_dummyarglist($2))

    type(xmlf_t), intent(inout) :: xf
dnl
m4_foreach(`x', `$2', `TOHWM4_dummyargdecl(x)')
dnl
 
    call xml_NewElement(xf, "$1List")
dnl
m4_foreach(`x', `$2', `TOHWM4_dummyarguse(x)')
dnl
    
  end subroutine cmlStart$1List

  subroutine cmlEnd$1List(xf)

    type(xmlf_t), intent(inout) :: xf

    Call xml_EndElement(xf, "$1List")
    
  end subroutine cmlEnd$1List
')dnl
dnl
dnl
dnl This is what a subroutine looks like if it is for a SCALAR quantity
dnl FIrst arg is name of quantity (property/parameter)
dnl Second arg is list of optional args
dnl Third arg is type of property(character/logical etc.)
define(`TOHWM4_ScalarSub',`dnl
  subroutine TOHWM4_subroutinename(`$1',`Sca',`$3',`') &
    (xf, value, TOHWM4_dummyarglist(`$2'), units dnl
ifelse(substr($3,0,4),`Real',`,fmt)',`)')

    type(xmlf_t), intent(inout)              :: xf
    TOHWM4_declarationtype(`$3'), intent(in) :: value
dnl
m4_foreach(`x', `$2', `TOHWM4_dummyargdecl(x)')
dnl
    character(len=*), intent(in), optional :: units
ifelse(substr($3,0,4),`Real',`dnl 
    character(len=*), intent(in), optional :: fmt
')dnl

    call xml_NewElement(xf, "$1")
dnl
m4_foreach(`x', `$2', `TOHWM4_dummyarguse(x)')
dnl
    call stmAddValue(xf=xf, value=value dnl
ifelse(`$3',`Lg',`',`, units=units') dnl
ifelse(substr($3,0,4),`Real',`,fmt=fmt',`') dnl
)
dnl
    call xml_EndElement(xf, "$1")

  end subroutine TOHWM4_subroutinename(`$1',`Sca',`$3',`')
')dnl
dnl
dnl
dnl
dnl Arrays; assumed shape/size:
dnl Same as before, but fourth arg is Sh/Si according
dnl to assumed shape/size
define(`TOHWM4_ArraySub',`dnl
  subroutine TOHWM4_subroutinename(`$1',`Arr',`$3',`$4') &
    (xf, value, dnl
ifelse($4, `Si', `nvalue, ') dnl
TOHWM4_dummyarglist(`$2'), units dnl
ifelse(substr($3,0,4),`Real',`,fmt)',`)')

    type(xmlf_t), intent(inout)              :: xf
ifelse($4, `Si', `dnl
    integer, intent(in)                      :: nvalue
')dnl
    TOHWM4_declarationtype(`$3'), intent(in) :: value(dnl
ifelse($4, `Si', `*', `:') dnl
)
dnl
m4_foreach(`x', `$2', `TOHWM4_dummyargdecl(x)')
dnl
    character(len=*), intent(in), optional :: units
ifelse(substr($3,0,4),`Real',`dnl 
    character(len=*), intent(in), optional :: fmt
')dnl

    call xml_NewElement(xf, "$1")
dnl
m4_foreach(`x', `$2', `TOHWM4_dummyarguse(x)')
dnl
ifelse($4, `Si', `dnl
    call stmAddValue(xf=xf, value=value(:nvalue) dnl
',`dnl
    call stmAddValue(xf=xf, value=value dnl
')dnl
ifelse(`$3',`Lg',`',`, units=units') dnl
ifelse(substr($3,0,4),`Real',`,fmt=fmt',`') dnl
)
dnl
    call xml_EndElement(xf, "$1")
  end subroutine TOHWM4_subroutinename(`$1',`Arr',`$3',`$4')
')dnl
dnl
dnl
dnl
dnl Matrices; assumed shape/size:
dnl Same as before, but fourth arg is Sh/Si according
dnl to assumed shape/size
define(`TOHWM4_MatrixSub',`dnl
  subroutine TOHWM4_subroutinename(`$1',`Mat',`$3',`$4') &
    (xf, value, dnl
ifelse($4, `Si', `nrows, ncols, ') dnl
TOHWM4_dummyarglist(`$2'), units dnl
ifelse(substr($3,0,4),`Real',`,fmt)',`)')

    type(xmlf_t), intent(inout)              :: xf
ifelse($4, `Si', `dnl
    integer, intent(in)                      :: nrows, ncols
')dnl
    TOHWM4_declarationtype(`$3'), intent(in) :: value(dnl
ifelse($4, `Si', `nrows, *', `:, :') dnl
)
dnl
m4_foreach(`x', `$2', `TOHWM4_dummyargdecl(x)')
dnl
    character(len=*), intent(in), optional :: units
ifelse(substr($3,0,4),`Real',`dnl 
    character(len=*), intent(in), optional :: fmt
')dnl

    call xml_NewElement(xf, "$1")
dnl
m4_foreach(`x', `$2', `TOHWM4_dummyarguse(x)')
dnl
ifelse($4, `Si', `dnl
    call stmAddValue(xf=xf, value=value(:nrows,:ncols) dnl
',`dnl
    call stmAddValue(xf=xf, value=value dnl
')dnl
ifelse(`$3',`Lg',`',`, units=units') dnl
ifelse(substr($3,0,4),`Real',`, fmt=fmt',`') dnl
)
    call xml_EndElement(xf, "$1")
  end subroutine TOHWM4_subroutinename(`$1',`Mat',`$3',`$4')
')dnl
dnl
dnl
dnl
dnl
define(`TOHWM4_Doc',`dnl
! This file is AUTOGENERATED!!!!
! Do not edit this file; edit m_wcml_quantity.m4 and regenerate.
!
!
module m_wcml_$1

  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement
  use m_wcml_stml, only: stmAddValue

  implicit none
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)

  interface cmlAdd$1
m4_foreach(`x', `(RealDp, RealSp, Int, Lg, Ch)', `TOHWM4_interfacelist($1, x)')
  end interface cmlAdd$1

  public :: cmlAdd$1
  public :: cmlStart$1List
  public :: cmlEnd$1List

contains

dnl Make the Start/End Lists
TOHWM4_QuantityList(`$1', `$2')

dnl Make the Scalar subroutines:
m4_foreach(`x', `(RealDp, RealSp, Int, Lg, Ch)', `TOHWM4_ScalarSub($1, `$2', x)
')
dnl Make the assumed size array subroutines:
m4_foreach(`x', `(RealDp, RealSp, Int, Lg, Ch)', `TOHWM4_ArraySub($1, `$2', x, `Si')
')
dnl Make the assumed shape array subroutines:
m4_foreach(`x', `(RealDp, RealSp, Int, Lg, Ch)', `TOHWM4_ArraySub($1, `$2', x, `Sh')
')
dnl Make the assumed size matrix subroutines:
m4_foreach(`x', `(RealDp, RealSp, Int, Lg, Ch)', `TOHWM4_MatrixSub($1, `$2', x, `Si')
')
dnl Make the assumed shape matrix subroutines:
m4_foreach(`x', `(RealDp, RealSp, Int, Lg, Ch)', `TOHWM4_MatrixSub($1, `$2', x, `Sh')
')
dnl
end module m_wcml_$1
')dnl
dnl
TOHWM4_Doc(QUANTITY, ARGS)
