module m_wcml_property

  use xmlf90_wxml, only: xmlf_t
  use xmlf90_wxml, only: xml_NewElement, xml_AddAttribute
  use xmlf90_wxml, only: xml_EndElement
  use m_wcml_stml, only: stmAddValue

  implicit none
  
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)

  public :: cmlAddProperty
  public :: cmlStartPropertyList
  public :: cmlEndPropertyList

  interface cmlAddProperty
     module procedure cmlAddPropScalarDP
     module procedure cmlAddPropScalarSP
     module procedure cmlAddPropScalarI
     module procedure cmlAddPropScalarCh
     module procedure cmlAddPropScalarLg
     module procedure cmlAddPropArrayDPSi
     module procedure cmlAddPropArrayDPSh
     module procedure cmlAddPropArraySPSi
     module procedure cmlAddPropArraySPSh
     module procedure cmlAddPropArrayISi
     module procedure cmlAddPropArrayISh
     module procedure cmlAddPropArrayChSi
     module procedure cmlAddPropArrayChSh
     module procedure cmlAddPropArrayLgSi
     module procedure cmlAddPropArrayLgSh
     module procedure cmlAddPropMatrixDPSi
     module procedure cmlAddPropMatrixDPSh
     module procedure cmlAddPropMatrixSPSi
     module procedure cmlAddPropMatrixSPSh
     module procedure cmlAddPropMatrixISi
     module procedure cmlAddPropMatrixISh
     module procedure cmlAddPropMatrixChSi
     module procedure cmlAddPropMatrixChSh
     module procedure cmlAddPropMatrixLgSi
     module procedure cmlAddPropMatrixLgSh
  end interface

contains

  subroutine cmlStartPropertyList(xf, id, title, conv, dictref, ref, role)

    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role
    
    call xml_NewElement(xf, 'propertyList')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    
  end subroutine cmlStartPropertyList

  subroutine cmlEndPropertyList(xf)

    type(xmlf_t), intent(inout) :: xf

    Call xml_EndElement(xf, 'propertyList')
    
  end subroutine cmlEndPropertyList

  subroutine cmlAddPropScalarDP(xf, property, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), intent(in)               :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))    call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')

  end subroutine cmlAddPropScalarDP

  subroutine cmlAddPropScalarSP(xf, property, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t),     intent(inout)        :: xf
    real(kind=sp),    intent(in)           :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropScalarSP
  
  subroutine cmlAddPropScalarI(xf, property, id, title, conv, dictref, ref, units)

    type(xmlf_t), intent(inout) :: xf
    integer, intent(in) :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property, units=units)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropScalarI

  subroutine cmlAddPropMatrixDPSi(xf, property, nrows, ncols, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: nrows
    integer, intent(in)                    :: ncols
    real(kind=dp), intent(in)              :: property(ncols, nrows)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(:ncols,:nrows), units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixDPSi

  subroutine cmlAddPropMatrixDPSh(xf, property, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: property(:,:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units
    
    integer :: nrows, ncols

    ncols=size(property, 1)
    nrows=size(property, 2)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(:ncols,:nrows), units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixDPSh

  subroutine cmlAddPropMatrixSPSi(xf, property, nrows, ncols, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: nrows
    integer, intent(in)                    :: ncols
    real(kind=sp), intent(in)              :: property(ncols,nrows)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf,value=property(:ncols,:nrows), units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixSPSi

  subroutine cmlAddPropMatrixSPSh(xf, property, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: property(:,:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    integer :: nrows, ncols

    ncols=size(property, 1)
    nrows=size(property, 2)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf,value=property(:ncols,:nrows), units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixSPSh

  subroutine cmlAddPropMatrixISi(xf, property, nrows, ncols, id, title, conv, dictref, ref, units)

    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: nrows
    integer, intent(in)                    :: ncols
    integer, intent(in)                    :: property(nrows,ncols)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(conv))    call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(:ncols,:nrows), units=units)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixISi

  subroutine cmlAddPropMatrixISh(xf, property, id, title, conv, dictref, ref, units)

    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: property(:,:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    integer :: nrows, ncols

    ncols=size(property, 2)
    nrows=size(property, 1)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(conv))    call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(:ncols,:nrows), units=units)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixISh

  subroutine cmlAddPropArrayDPSi(xf, property, nvalue, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: property(*)
    integer, intent(in)                    :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(1:nvalue), units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArrayDPSi

  subroutine cmlAddPropArrayDPSh(xf, property, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: property(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArrayDPSh

  subroutine cmlAddPropArraySPSi(xf, property, nvalue, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: property(*)
    integer, intent(in)                    :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(1:nvalue), units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArraySPSi

  subroutine cmlAddPropArraySPSh(xf, property, id, title, conv, dictref, ref, units, fmt)

    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)              :: property(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArraySPSh

  subroutine cmlAddPropArrayISi(xf, property, nvalue, id, title, conv, dictref, ref, units)

    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: property(*)
    integer, intent(in)                    :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf, value=property(1:nvalue), units=units)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArrayISi

  subroutine cmlAddPropArrayISh(xf, property, id, title, conv, dictref, ref, units)

    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: property(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf, value=property, units=units)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArrayISh

  subroutine cmlAddPropScalarCH(xf, property, id, title, conv, dictref, ref, units)

    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in)           :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property, units=units)
    call xml_EndElement(xf, 'property')

  end subroutine cmlAddPropScalarCH

  subroutine cmlAddPropMatrixCHSi(xf, property, nrows, ncols, id, title, conv, dictref, ref, units)

    type(xmlf_t),     intent(inout)        :: xf
    integer,          intent(in)           :: nrows
    integer,          intent(in)           :: ncols
    character(len=*), intent(in)           :: property(ncols,nrows)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(:ncols,:nrows), units=units)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixCHSi


  subroutine cmlAddPropMatrixCHSh(xf, property, id, title, conv, dictref, ref, units)

    type(xmlf_t),     intent(inout)        :: xf
    character(len=*), intent(in)           :: property(:,:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    integer :: nrows, ncols

    ncols=size(property, 1)
    nrows=size(property, 2)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(:ncols,:nrows), units=units)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixCHSh

  subroutine cmlAddPropArrayCHSi(xf, property, nvalue, id, title, conv, dictref, ref)

    type(xmlf_t),     intent(inout)        :: xf
    character(len=*), intent(in)           :: property(*)
    integer,          intent(in)           :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf, value=property(1:nvalue))
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArrayCHSi

  subroutine cmlAddPropArrayCHSh(xf, property, id, title, conv, dictref, ref)

    type(xmlf_t),     intent(inout)        :: xf
    character(len=*), intent(in)           :: property(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref

    integer :: nvalue

    nvalue=size(property)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf, value=property)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArrayCHSh

  subroutine cmlAddPropScalarLG(xf, property, id, title, conv, dictref, ref, units)

    type(xmlf_t), intent(inout) :: xf
    logical,          intent(in)           :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property, units=units)
    call xml_EndElement(xf, 'property')

  end subroutine cmlAddPropScalarLG

  subroutine cmlAddPropArrayLgSh(xf, property, id, title, conv, dictref, ref)

    type(xmlf_t),     intent(inout)        :: xf
    logical,          intent(in)           :: property(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref

    integer :: nvalue

    nvalue=size(property)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf, value=property)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArrayLgSh

  subroutine cmlAddPropArrayLgSi(xf, property, nvalue, id, title, conv, dictref, ref)

    type(xmlf_t),     intent(inout)        :: xf
    logical,          intent(in)           :: property(*)
    integer,          intent(in)           :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf, value=property(:nvalue))
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropArrayLgSi

  subroutine cmlAddPropMatrixLgSh(xf, property, id, title, conv, dictref, ref, units)

    type(xmlf_t),     intent(inout)        :: xf
    logical,          intent(in)           :: property(:,:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    integer :: nrows, ncols

    ncols=size(property, 1)
    nrows=size(property, 2)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(:ncols,:nrows), units=units)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixLgSh

  subroutine cmlAddPropMatrixLgSi(xf, nrows, ncols, property, id, title, conv, dictref, ref, units)

    type(xmlf_t),     intent(inout)        :: xf
    integer,          intent(in)           :: nrows
    integer,          intent(in)           :: ncols
    logical,          intent(in)           :: property(ncols, nrows)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddValue(xf=xf, value=property(:ncols,:nrows), units=units)
    call xml_EndElement(xf, 'property')
  end subroutine cmlAddPropMatrixLgSi

end module m_wcml_property
