module m_wcml_stml

  use m_common_format, only : str
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_AddCharacters, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement

  implicit none
  private

  integer, private, parameter ::  sp = selected_real_kind(6,30)
  integer, private, parameter ::  dp = selected_real_kind(14,100)

  public :: stmAddValue
  public :: stmAddScalar
  public :: stmAddArray
  public :: stmAddMatrix

  interface stmAddValue
     module procedure stmAddString
     module procedure stmAddInteger
     module procedure stmAddFloatSP
     module procedure stmAddFloatDP
     module procedure stmAddLogical
     module procedure stmAddStringArray
     module procedure stmAddIntegerArray
     module procedure stmAddFloatArraySP
     module procedure stmAddFloatArrayDP
     module procedure stmAddLogicalArray
     module procedure stmAddStringMatrix
     module procedure stmAddIntegerMatrix
     module procedure stmAddFloatMatrixSP
     module procedure stmAddFloatMatrixDP
     module procedure stmAddLogicalMatrix
  end interface stmAddValue

  interface stmAddScalar
     module procedure stmAddString
     module procedure stmAddInteger
     module procedure stmAddFloatSP
     module procedure stmAddFloatDP
     module procedure stmAddLogical
  end interface

  interface stmAddArray
     module procedure stmAddStringArray
     module procedure stmAddIntegerArray
     module procedure stmAddFloatArraySP
     module procedure stmAddFloatArrayDP
     module procedure stmAddLogicalArray
  end interface

  interface stmAddMatrix
     module procedure stmAddStringMatrix
     module procedure stmAddIntegerMatrix
     module procedure stmAddFloatMatrixSP
     module procedure stmAddFloatMatrixDP
     module procedure stmAddLogicalMatrix
  end interface

contains
  
  
  ! =================================================
  ! STMML convenience routines
  ! =================================================

!FIXME TOHW temp fix until we move stuff about ..

  subroutine cmlAddAttribute(xf, attr, valu)
    type(xmlf_t), intent(inout):: xf
    character(len=*), intent(in) :: attr, valu

    call xml_AddAttribute(xf, attr, valu)
  end subroutine cmlAddAttribute
  
  ! -------------------------------------------------
  ! outputs STMML scalar in xml channel
  ! -------------------------------------------------

  subroutine stmAddString(xf, value, id, title, dictref, dataType, &
       convention, errorValue, errorBasis, min, max, ref, units)

    type(xmlf_t),     intent(inout)        :: xf
    character(len=*), intent(in)           :: value         ! the value to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: dataType  
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    !FIXME checkAttributeValues('id', id)

    call xml_NewElement(xf, 'scalar')
    if (present(id))         call cmlAddAttribute(xf, 'id', id)
    if (present(title))      call cmlAddAttribute(xf, 'title', title)
    if (present(dictref))    call cmlAddAttribute(xf, 'dictRef', dictref)
    if (present(dataType)) then
      call cmlAddAttribute(xf, 'dataType', dataType)
    else
      call cmlAddAttribute(xf, 'dataType', 'xsd:string')
    endif
    if (present(convention)) call cmlAddAttribute(xf, 'convention', convention)
    if (present(errorValue)) call cmlAddAttribute(xf, 'errorValue', errorValue)
    if (present(errorBasis)) call cmlAddAttribute(xf, 'errorBasis', errorBasis)
    if (present(min))        call cmlAddAttribute(xf, 'min', min)
    if (present(max))        call cmlAddAttribute(xf, 'max', max)
    if (present(ref))        call cmlAddAttribute(xf, 'ref', ref)
    if (present(units))      call cmlAddAttribute(xf, 'units', units)
    call xml_AddCharacters(xf, value)
    call xml_EndElement(xf, 'scalar')

  end subroutine stmAddString

  subroutine stmAddLogical(xf, value, id, title, dictref, &
       convention, errorValue, errorBasis, min, max, ref, units)

    type(xmlf_t),     intent(inout)        :: xf
    logical,          intent(in)           :: value        ! the value to be output
    character(len=*), intent(in), optional :: id           ! the id
    character(len=*), intent(in), optional :: title        ! the title
    character(len=*), intent(in), optional :: dictref      ! the dictionary reference
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units        ! units (default = none)

    call stmAddString(xf=xf, value=str(value), id=id, title=title, &
                      dictRef=dictref, dataType='xsd:boolean', &
                      convention=convention, errorValue=errorValue, &
                      errorBasis=errorBasis, min=min, max=max, ref=ref, &
                      units=units)

  end subroutine stmAddLogical

  subroutine stmAddInteger(xf, value, id, title, dictref, &
       convention, errorValue, errorBasis, min, max, ref, units)

    type(xmlf_t),     intent(inout)        :: xf
    integer,          intent(in)           :: value        ! the value to be output
    character(len=*), intent(in), optional :: id           ! the id
    character(len=*), intent(in), optional :: title        ! the title
    character(len=*), intent(in), optional :: dictref      ! the dictionary reference
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units        ! units (default = none)

    call stmAddString(xf=xf, value=str(value), id=id, title=title, &
                      dictRef=dictref, dataType='xsd:integer', &
                      convention=convention, errorValue=errorValue, &
                      errorBasis=errorBasis, min=min, max=max, ref=ref, &
                      units=units)

  end subroutine stmAddInteger

  subroutine stmAddFloatDP(xf, value, id, title, dictref, &
       convention, errorValue, errorBasis, min, max, ref, units, fmt)

    type(xmlf_t),     intent(inout)        :: xf
    real(kind=dp),    intent(in)           :: value        ! the value to be output
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! the title
    character(len=*), intent(in), optional :: dictref      ! the dictionary reference
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units        ! units
    character(len=*), intent(in), optional :: fmt 

    if (present(fmt)) then
       call stmAddString(xf=xf, value=str(value, fmt), id=id, title=title, &
                      dictRef=dictref, dataType='xsd:double', &
                      convention=convention, errorValue=errorValue, &
                      errorBasis=errorBasis, min=min, max=max, ref=ref, &
                      units=units)
    else
       call stmAddString(xf=xf, value=str(value), id=id, title=title, &
                      dictRef=dictref, dataType='xsd:double', &
                      convention=convention, errorValue=errorValue, &
                      errorBasis=errorBasis, min=min, max=max, ref=ref, &
                      units=units)
    endif

  end subroutine stmAddFloatDP

  subroutine stmAddFloatSP(xf, value, id, title, dictref, &
       convention, errorValue, errorBasis, min, max, ref, units, fmt)

    type(xmlf_t),  intent(inout)            :: xf
    real(kind=sp), intent(in)               :: value        ! the value to be output
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! the title
    character(len=*), intent(in), optional :: dictref      ! the dictionary reference
    character(len=*), intent(in), optional :: units        ! units (' ' = none)
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: errorValue
    character(len=*), intent(in), optional :: errorBasis
    character(len=*), intent(in), optional :: min
    character(len=*), intent(in), optional :: max
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt 

    if (present(fmt)) then
       call stmAddString(xf=xf, value=str(value, fmt), id=id, title=title, &
                      dictRef=dictref, dataType='xsd:float', &
                      convention=convention, errorValue=errorValue, &
                      errorBasis=errorBasis, min=min, max=max, ref=ref, &
                      units=units)
    else
       call stmAddString(xf=xf, value=str(value), id=id, title=title, &
                      dictRef=dictref, dataType='xsd:float', &
                      convention=convention, errorValue=errorValue, &
                      errorBasis=errorBasis, min=min, max=max, ref=ref, &
                      units=units)
    endif

  end subroutine stmAddFloatSP


  ! -------------------------------------------------
  ! outputs string array to xml channel
  ! -------------------------------------------------

  subroutine stmStartArrayTag(xf, nvalue, id, title, dictref, dataType, delim, ref, units)

    type(xmlf_t) :: xf
    integer, intent(in)                    :: nvalue        ! number of values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: dataType      ! the dataType
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units
    character(len=1), intent(in), optional :: delim         ! delimiter

    call xml_NewElement(xf, 'array')
    call xml_AddAttribute(xf, 'size', nvalue)
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(dataType))    call xml_AddAttribute(xf, 'dataType', dataType)
    if (present(ref))    call xml_AddAttribute(xf, 'ref', ref)
    if (present(units))    call xml_AddAttribute(xf, 'units', units)
    if (present(delim)) then
      call xml_AddAttribute(xf, 'delimiter', delim)
    else
      call xml_AddAttribute(xf, 'delimiter', ' ')
    endif

  end subroutine stmStartArrayTag

  subroutine stmAddStringArray(xf, value, id, title, dictref, dataType, delim, ref, units)

    type(xmlf_t) :: xf
    character(len=*), intent(in)           :: value(:)      ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: dataType      ! the dataType
    character(len=*), intent(in), optional :: ref 
    character(len=*), intent(in), optional :: units
    character(len=1), intent(in), optional :: delim         ! delimiter

    integer :: nvalue
    character(len=1) :: delim1

    nvalue = size(value)

    if (present(delim)) then
       delim1 = delim
    else
       delim1 = ' '
    endif

    if (present(dataType)) then
      call stmStartArrayTag(xf=xf, nvalue=nvalue, id=id, title=title, &
        dictRef=dictRef, dataType=dataType, ref=ref, delim=delim1, &
        units=units)
    else
      call stmStartArrayTag(xf=xf, nvalue=nvalue, id=id, title=title, &
        dictRef=dictRef, dataType='xsd:string', ref=ref, delim=delim1, &
        units=units)
    endif

    call xml_AddCharacters(xf, value, delim1)

    call xml_EndElement(xf, 'array')

  end subroutine stmAddStringArray

  subroutine stmAddLogicalArray(xf, value, id, title, dictref, ref)

    type(xmlf_t),     intent(inout)        :: xf
    logical,          intent(in)           :: value(:)      ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: ref           ! delimiter

    integer          :: nvalue

    nvalue = size(value)
    call stmStartArrayTag(xf=xf, nvalue=nvalue, id=id, title=title, dictRef=dictRef, dataType='xsd:boolean', ref=ref)

    call xml_AddCharacters(xf, value)

    call xml_EndElement(xf, 'array')

  end subroutine stmAddLogicalArray

  subroutine stmAddIntegerArray(xf, value, id, title, dictref, ref, units)

    type(xmlf_t),     intent(inout)        :: xf
    integer,          intent(in)           :: value(:)      ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: ref

    integer          :: nvalue

    nvalue = size(value)
    call stmStartArrayTag(xf=xf, nvalue=nvalue, id=id, title=title, dictRef=dictRef, dataType='xsd:integer', ref=ref, units=units)

    call xml_AddCharacters(xf, value)

    call xml_EndElement(xf, 'array')

  end subroutine stmAddIntegerArray

  subroutine stmAddFloatArraySP(xf, value, id, title, dictref, units, ref, fmt)

    type(xmlf_t),     intent(inout)        :: xf
    real(kind=sp),    intent(in)           :: value(:)      ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: fmt           ! the output format
    character(len=*), intent(in), optional :: ref

    integer          :: nvalue

    nvalue = size(value)
    call stmStartArrayTag(xf=xf, nvalue=nvalue, id=id, title=title, dictRef=dictRef, dataType='xsd:float', ref=ref, units=units)

    call xml_AddCharacters(xf, value, fmt)

    call xml_EndElement(xf, 'array')

  end subroutine stmAddFloatArraySP

  subroutine stmAddFloatArrayDP(xf, value, id, title, dictref, units, ref, fmt)

    type(xmlf_t),     intent(inout)        :: xf
    real(kind=dp),    intent(in)           :: value(:)      ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: ref           ! the output format
    character(len=*), intent(in), optional :: fmt           ! the output format

    integer          :: nvalue

    nvalue = size(value)
    call stmStartArrayTag(xf=xf, nvalue=nvalue, id=id, title=title, dictRef=dictRef, dataType='xsd:double', ref=ref, units=units)

    call xml_AddCharacters(xf, value, fmt)

    call xml_EndElement(xf, 'array')

  end subroutine stmAddFloatArrayDP

  ! -------------------------------------------------
  ! outputs integer matrix to xml channel
  ! -------------------------------------------------

  subroutine stmStartMatrixTag(xf, nrows, ncols, id, title, dictRef, units, dataType)

    type(xmlf_t),     intent(inout)        :: xf
    integer,          intent(in)           :: nrows
    integer,          intent(in)           :: ncols
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: dataType

    call xml_NewElement(xf, 'matrix')
    call xml_AddAttribute(xf, 'rows', nrows)
    call xml_AddAttribute(xf, 'columns', ncols)
    if (present(id))        call xml_AddAttribute(xf, 'id', id)
    if (present(dictref))   call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(title))     call xml_AddAttribute(xf, 'title', title)
    if (present(units))     call xml_AddAttribute(xf, 'units', units)
    if (present(dataType))  call xml_AddAttribute(xf, 'dataType', dataType)

  end subroutine stmStartMatrixTag

  subroutine stmAddStringMatrix(xf, value, id, title, dictref, units, dataType)

    type(xmlf_t),     intent(inout)        :: xf
    character(len=*), intent(in)           :: value(:,:)   ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: dataType

    integer ::  nrows, ncols

    ncols = size(value, 1)
    nrows = size(value, 2)

    if (present(dataType)) then
      call stmStartMatrixTag(xf=xf, nrows=nrows, ncols=ncols, id=id, &
                             title=title, dictRef=dictRef, units=units, dataType=dataType)
    else
      call stmStartMatrixTag(xf=xf, nrows=nrows, ncols=ncols, id=id, &
                             title=title, dictRef=dictRef, units=units, dataType='xsd:string')
    endif

    call xml_AddCharacters(xf, reshape(value, (/ncols*nrows/)))

    call xml_EndElement(xf, 'matrix')

  end subroutine stmAddstringMatrix

  subroutine stmAddIntegerMatrix(xf, value, id, title, dictref, units)

    type(xmlf_t),     intent(inout)        :: xf
    integer,          intent(in)           :: value(:,:)   ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')

    integer :: nrows, ncols

    ncols = size(value, 1)
    nrows = size(value, 2)

    call stmStartMatrixTag(xf=xf, nrows=nrows, ncols=ncols, id=id, &
                           title=title, dictRef=dictRef, units=units, dataType='xsd:integer')

    call xml_AddCharacters(xf, reshape(value, (/ncols*nrows/)))

    call xml_EndElement(xf, 'matrix')

  end subroutine stmAddIntegerMatrix

  subroutine stmAddLogicalMatrix(xf, value, id, title, dictref, units)

    type(xmlf_t),     intent(inout)        :: xf
    logical,          intent(in)           :: value(:,:)   ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')

    integer :: nrows, ncols

    ncols = size(value, 1)
    nrows = size(value, 2)

    call stmStartMatrixTag(xf=xf, nrows=nrows, ncols=ncols, id=id, &
                           title=title, dictRef=dictRef, units=units, dataType='xsd:boolean')


    call xml_AddCharacters(xf, reshape(value, (/ncols*nrows/)))

    call xml_EndElement(xf, 'matrix')

  end subroutine stmAddLogicalMatrix

  subroutine stmAddFloatMatrixDP(xf, value, id, title, dictref, units, fmt)

    type(xmlf_t),     intent(inout)        :: xf
    real(dp),         intent(in)           :: value(:,:)   ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: fmt

    integer :: nrows, ncols

    ncols = size(value, 1)
    nrows = size(value, 2)

    call stmStartMatrixTag(xf=xf, nrows=nrows, ncols=ncols, id=id, &
                           title=title, dictRef=dictRef, units=units, dataType='xsd:double')


    call xml_AddCharacters(xf, reshape(value, (/ncols*nrows/)), fmt)

    call xml_EndElement(xf, 'matrix')

  end subroutine stmAddFloatMatrixDP

  subroutine stmAddFloatMatrixSP(xf, value, id, title, dictref, units, fmt)

    type(xmlf_t),     intent(inout)        :: xf
    real(sp),         intent(in)           :: value(:,:)   ! the values to be output
    character(len=*), intent(in), optional :: id            ! the id
    character(len=*), intent(in), optional :: title         ! the title
    character(len=*), intent(in), optional :: dictref       ! the dictionary reference
    character(len=*), intent(in), optional :: units         ! scienitific units (default ' ')
    character(len=*), intent(in), optional :: fmt

    integer :: nrows, ncols

    ncols = size(value, 1)
    nrows = size(value, 2)

    call stmStartMatrixTag(xf=xf, nrows=nrows, ncols=ncols, id=id, &
                           title=title, dictRef=dictRef, units=units, dataType='xsd:float')

    call xml_AddCharacters(xf, reshape(value, (/ncols*nrows/)), fmt)

    call xml_EndElement(xf, 'matrix')

  end subroutine stmAddFloatMatrixSP

end module m_wcml_stml
