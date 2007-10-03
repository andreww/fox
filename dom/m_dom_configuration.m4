TOHW_m_dom_publics(`

  integer, parameter :: configParamLen = 42

  character(len=configParamLen), parameter :: configParams(26) = (/ &
    ! DOM 3 Core:
    "canonical-form                           ", &
    "cdata-sections                           ", &
    "check-character-normalization            ", &
    "comments                                 ", &
    "datatype-normalization                   ", &
    "element-content-whitespace               ", &
    "entities                                 ", &
    "error-handler                            ", &
    "infoset                                  ", &
    "namespaces                               ", &
    "namespace-declarations                   ", &
    "normalize-characters                     ", &
    "schema-location                          ", &
    "schema-type                              ", &
    "split-cdata-sections                     ", &
    "validate                                 ", &
    "validate-if-schema                       ", &
    "well-formed                              ", &
    ! DOM 3 LS (Parser):
    "charset-overrides-xml-encoding           ", &
    "disallow-doctype                         ", &
    "ignore-unknown-character-denormalizations", &
    "resource-resolver                        ", &
    "supported-media-types-only               ", &
    ! DOM 3 LS (Serializer)
    "discard-default-content                  ", &
    "format-pretty-print                      ", &
    "xml-declaration                          " /)

!!$  logical, parameter :: paramSettable(26) = (/ &
!!$    .false., & ! canonical-form
!!$    .true.,  & ! cdata-sections
!!$    .false., & ! check-character-normalization
!!$    .true.,  & ! comments
!!$    .false., & ! datatype-normalization
!!$    .true.,  & ! element-content-whitespace
!!$    .true.,  & ! entities
!!$    .false., & ! error-handler BREACH OF SPEC
!!$    .false., & ! infoset
!!$    .true.,  & ! namespaces
!!$    .true.,  & ! namespace-declarations
!!$    .false., & ! normalize-characters
!!$    .true.,  & ! split-cdata-sections
!!$    .true.,  & ! validate
!!$    .true.,  & ! validate-if-schema
!!$    .false., & ! well-formed
!!$    .false., & ! charset-overrides-xml-encoding
!!$    .false., & ! disallow-doctype
!!$    .false., & ! ignore-unknown-character-denormalizations
!!$    .false., & ! resource-resolver BREACH OF SPEC
!!$    .false., & ! supported-media-types-only
!!$    .true.,  & ! discard-default-content
!!$    .false., & ! format-pretty-print
!!$    .true.  /) ! xml-declaration
  integer, parameter :: paramSettable = 121695444

!!$  logical, parameter :: paramDefaults(26) = (/ &
!!$    .false., & ! canonical-form
!!$    .true.,  & ! cdata-sections
!!$    .false., & ! check-character-normalization
!!$    .true.,  & ! comments
!!$    .false., & ! datatype-normalization
!!$    .true.,  & ! element-content-whitespace
!!$    .true.,  & ! entities
!!$    .false., & ! error-handler BREACH OF SPEC
!!$    .true.,  & ! infoset
!!$    .true.,  & ! namespaces
!!$    .true.,  & ! namespace-declarations
!!$    .false., & ! normalize-characters
!!$    .true.,  & ! split-cdata-sections
!!$    .false., & ! validate
!!$    .false., & ! validate-if-schema
!!$    .true.,  & ! well-formed
!!$    .false., & ! charset-overrides-xml-encoding
!!$    .false., & ! disallow-doctype
!!$    .true.,  & ! ignore-unknown-character-denormalizations
!!$    .false., & ! resource-resolver BREACH OF SPEC
!!$    .false., & ! supported-media-types-only
!!$    .true.,  & ! discard-default-content
!!$    .false., & ! format-pretty-print
!!$    .true.  /) ! xml-declaration
  integer, parameter :: paramDefaults = 55127764

  type DOMConfiguration
    private
    integer :: parameters = paramDefaults
    ! FIXME make sure this is 32 bit at least.
  end type DOMConfiguration

  public :: setParameter
  public :: getParameter
  public :: canSetParameter
  public :: getParameterNames

')`'dnl
dnl
TOHW_m_dom_contents(`

  recursive TOHW_subroutine(setParameter, (domConfig, name, value))
    type(DOMConfiguration), pointer :: domConfig
    character(len=*), intent(in) :: name
    logical, intent(in) :: value

    integer :: i, n
    do i = 1, size(configParams)
      if (name==trim(configParams(i))) then
        n = i
        exit
      endif
    enddo
    if (i > size(configParams)) then
      TOHW_m_dom_throw_error(NOT_FOUND_ERR)
    endif
    if (.not.canSetParameter(domConfig, name, value)) then
      TOHW_m_dom_throw_error(NOT_SUPPORTED_ERR)
    endif
    if (value) then
      domConfig%parameters = ibset(domConfig%parameters, n)
    else
      domConfig%parameters = ibclr(domConfig%parameters, n)
    endif

    select case (trim(name))
      !case ("canonical-form")
      !case ("infoset")
    case("validate")
      if (value) call setParameter(domConfig, "validate-if-schema", .false.)
    case ("validate-if-schema")
      if (value) call setParameter(domConfig, "validate", .false.)
    end select

  end subroutine setParameter

  TOHW_function(getParameter, (domConfig, name), value)
    type(DOMConfiguration), pointer :: domConfig
    character(len=*), intent(in) :: name
    logical :: value

    integer :: i, n
    do i = 1, size(configParams)
      if (name==trim(configParams(i))) then
        n = i
        exit
      endif
    enddo
    if (i > size(configParams)) then
      TOHW_m_dom_throw_error(NOT_FOUND_ERR)
    endif

    value = btest(domConfig%parameters, n)

  end function getParameter

  TOHW_function(canSetParameter, (domConfig, name, value), p)
    type(DOMConfiguration), pointer :: domConfig
    character(len=*), intent(in) :: name
    logical, intent(in) :: value

    logical :: p
    integer :: i, n
    do i = 1, size(configParams)
      if (name==trim(configParams(i))) then
        n = i
        exit
      endif
    enddo
    if (i > size(configParams)) then
      p = .false.
      return
    endif

    p = btest(paramSettable, n)

  end function canSetParameter

  TOHW_function(getParameterNames, (domConfig), s)
    type(DOMConfiguration), pointer :: domConfig
    character(len=configParamLen) :: s(size(configParams))

    s = configParams
  end function getParameterNames

')`'dnl
