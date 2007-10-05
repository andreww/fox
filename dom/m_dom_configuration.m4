dnl This file sort-of implements the DOMConfiguration part of 
dnl DOM Level 3 Core + LS.
dnl It falls short of the spec because:
dnl
dnl a) it only deals with configuration options which have boolean values.
dnl    (because getParameter must return only one kind of data)
dnl    This is fine except for error_handler, which ought to hold an error-handler
dnl    (which is compulsory, so we are in breach of spec)
dnl    and schema-location/schema-type which ought to hold strings)
dnl    (but support is optional, so we exclude)
dnl
dnl b) There should be a DOMConfiguration object for documents and for
dnl    serializers and parsers, each of which has a different list of 
dnl    available options. Since we dont implement LS serializers
dnl    parsers per se at the moment, all the options for all three are
dnl    understood by the same DOMConfiguration object.
dnl
dnl
TOHW_m_dom_publics(`

  integer, parameter :: configParamLen = 42

  character(len=configParamLen), parameter :: configParams(23) = (/ &
    ! DOM 3 Core:
    "canonical-form                           ", &
    "cdata-sections                           ", &
    "check-character-normalization            ", &
    "comments                                 ", &
    "datatype-normalization                   ", &
    "element-content-whitespace               ", &
    "entities                                 ", &
    "error-handler                            ", &
!    "infoset                                  ", & is not a real config option
    "namespaces                               ", &
    "namespace-declarations                   ", &
    "normalize-characters                     ", &
!    "schema-location                          ", & we dont implement
!    "schema-type                              ", & we dont implement
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
!!$    .true., & ! canonical-form
!!$    .true.,  & ! cdata-sections
!!$    .false., & ! check-character-normalization
!!$    .true.,  & ! comments
!!$    .false., & ! datatype-normalization
!!$    .true.,  & ! element-content-whitespace
!!$    .true.,  & ! entities
!!$    .false., & ! error-handler BREACH OF SPEC
!!$    !.false., & ! infoset
!!$    .true.,  & ! namespaces
!!$    .true.,  & ! namespace-declarations
!!$    .false., & ! normalize-characters
!!$    .true.,  & ! split-cdata-sections
!!$    .false., & ! well-formed
!!$    .false., & ! charset-overrides-xml-encoding
!!$    .false., & ! disallow-doctype
!!$    .false., & ! ignore-unknown-character-denormalizations
!!$    .false., & ! resource-resolver BREACH OF SPEC
!!$    .false., & ! supported-media-types-only
!!$    .true.,  & ! discard-default-content
!!$    .false., & ! format-pretty-print
!!$    .true.  /) ! xml-declaration
  integer, parameter :: paramSettable = 127956694

!!$  logical, parameter :: paramDefaults(26) = (/ &
!!$    .false., & ! canonical-form
!!$    .true.,  & ! cdata-sections
!!$    .false., & ! check-character-normalization
!!$    .true.,  & ! comments
!!$    .false., & ! datatype-normalization
!!$    .true.,  & ! element-content-whitespace
!!$    .true.,  & ! entities
!!$    .false., & ! error-handler BREACH OF SPEC
!!$    !.true.,  & ! infoset
!!$    .true.,  & ! namespaces
!!$    .true.,  & ! namespace-declarations
!!$    .false., & ! normalize-characters
!!$    .true.,  & ! split-cdata-sections
!!$    .true.,  & ! well-formed
!!$    .false., & ! charset-overrides-xml-encoding
!!$    .false., & ! disallow-doctype
!!$    .true.,  & ! ignore-unknown-character-denormalizations
!!$    .false., & ! resource-resolver BREACH OF SPEC
!!$    .false., & ! supported-media-types-only
!!$    .true.,  & ! discard-default-content
!!$    .false., & ! format-pretty-print
!!$    .true.  /) ! xml-declaration
  integer, parameter :: paramDefaults = 94672596

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

  subroutine resetParameter(domConfig, name)
    type(DOMConfiguration), pointer :: domConfig
    character(len=*), intent(in) :: name

    type(DOMConfiguration) :: default
    integer :: i, n
    do i = 1, size(configParams)
      if (toLower(name)==trim(configParams(i))) then
        n = i
        exit
      endif
    enddo
    if (i>size(configParams)) return
    if (.not.btest(paramSettable, n)) return
    if (btest(paramDefaults, n)) then
      domConfig%parameters = ibset(domConfig%parameters, n)
    else
      domConfig%parameters = ibclr(domConfig%parameters, n)
    endif
  end subroutine resetParameter

  recursive TOHW_subroutine(setParameter, (domConfig, name, value))
    type(DOMConfiguration), pointer :: domConfig
    character(len=*), intent(in) :: name
    logical, intent(in) :: value
    integer :: i, n

    if (toLower(name)=="infoset") then
      if (value) then
        call setParameter(domConfig, "validate-if-schema", .false.)
        call setParameter(domConfig, "entities", .false.)
        ! cant do datatype-normalization
        call setParameter(domConfig, "cdata-sections", .false.)
        call setParameter(domConfig, "namespace-declarations", .true.)
        ! well-formed cannot be changed
        call setParameter(domConfig, "element-content-whitespace", .true.)
        call setParameter(domConfig, "comments", .true.)
        call setParameter(domConfig, "namespaces", .true.)
      endif
      return
    endif

    do i = 1, size(configParams)
      if (toLower(name)==trim(configParams(i))) then
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

    select case (toLower(name))
    case ("canonical-form")
      if (value) then
        domConfig%parameters = ibclr(domConfig%parameters, 7)
        ! cant do normalize-characters
        domConfig%parameters = ibclr(domConfig%parameters, 2)
        domConfig%parameters = ibset(domConfig%parameters, 9)
        domConfig%parameters = ibset(domConfig%parameters, 10)
        ! well-formed cannot be changed
        domConfig%parameters = ibset(domConfig%parameters, 6)
        ! FIXME when we work out pretty-print/preserve-whitespace semantics
        ! call setParameter(domConfig, "format-pretty-print", .false.)
        domConfig%parameters = ibclr(domConfig%parameters, 21)
        domConfig%parameters = ibclr(domConfig%parameters, 23)
      else
        call resetParameter(domConfig, "entities")
        ! cant do normalize-characters
        call resetParameter(domConfig, "cdata-sections")
        call resetParameter(domConfig, "namespaces")
        call resetParameter(domConfig, "namespace-declarations")
        ! well-formed cannot be changed
        call resetParameter(domConfig, "element-content-whitespace")
        call resetParameter(domConfig, "format-pretty-print")
        call resetParameter(domConfig, "discard-default-content")
        call resetParameter(domConfig, "xml-declaration")
      endif
    case ("cdata-sections")
      if (value) domConfig%parameters = ibclr(domConfig%parameters, 1)
    case ("element-content-whitespace")
      if (.not.value) domConfig%parameters = ibclr(domConfig%parameters, 1)
    case ("entities")
      if (value) domConfig%parameters = ibclr(domConfig%parameters, 1)
    case ("namespaces")
      if (.not.value) domConfig%parameters = ibclr(domConfig%parameters, 1)
    case ("namespaces-declarations")
      if (.not.value) domConfig%parameters = ibclr(domConfig%parameters, 1)
    case("validate")
      if (value) domConfig%parameters = ibclr(domConfig%parameters, 14)
    case ("validate-if-schema")
      if (value) domConfig%parameters = ibclr(domConfig%parameters, 13)
    case ("format-pretty-print")
      if (value) domConfig%parameters = ibclr(domConfig%parameters, 1)
    case ("discard-default-content")
      if (value) domConfig%parameters = ibclr(domConfig%parameters, 1)
    case ("xml-declaration")
      if (value) domConfig%parameters = ibclr(domConfig%parameters, 1)
    end select

  end subroutine setParameter

  recursive TOHW_function(getParameter, (domConfig, name), value)
    type(DOMConfiguration), pointer :: domConfig
    character(len=*), intent(in) :: name
    logical :: value

    integer :: i, n

    if (toLower(name)=="infoset") then
      value = &
        .not.getParameter(domConfig, "validate-if-schema") &
        .and..not.getParameter(domConfig, "entities") &
        .and..not.getParameter(domConfig, "datatype-normalization") &
        .and..not.getParameter(domConfig, "cdata-sections") &
        .and.getParameter(domConfig, "namespace-declarations") &
        .and.getParameter(domConfig, "well-formed") &
        .and.getParameter(domConfig, "element-content-whitespace") &
        .and.getParameter(domConfig, "comments") &
        .and.getParameter(domConfig, "namespaces")
      return
    endif

    do i = 1, size(configParams)
      if (toLower(name)==trim(configParams(i))) then
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

    if (toLower(name)=="infoset") then
      p = .true.
      return
    endif
    do i = 1, size(configParams)
      if (toLower(name)==trim(configParams(i))) then
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
