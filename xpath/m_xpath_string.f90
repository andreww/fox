module m_xpath_string

  !Module containing all string-related functions of xpath

  use m_xpath_types, only : xpath_type
  use m_xpath_types, only : XMLF90_XPATH_TYPE_NODESET, XMLF90_XPATH_TYPE_BOOLEAN
  use m_xpath_types, only : XMLF90_XPATH_TYPE_NUMBER, XMLF90_XPATH_TYPE_STRING
  use m_xpath_types, only : xpath_cast, stringify
  use m_xpath_types, only : contextNode

  use m_xpath_error, only : xpath_assert

  use m_strings, only : string, len, index, substr
  use m_strings, only : assignment(=), operator(+)

  implicit none
  
  private

  public :: xpath_string
  public :: xpath_concat
  public :: xpath_starts_with
  public :: xpath_contains
  public :: xpath_substring_before
  public :: xpath_substring_after
  public :: xpath_substring
  public :: xpath_string_length
  public :: xpath_normalize_space
  public :: xpath_translate
  
contains

  function xpath_string(xtin) result(xtout)
    type(xpath_type), optional, intent(in) :: xtin
    type(xpath_type) :: xtout

    xtout%type = XMLF90_XPATH_TYPE_STRING

    if (present(xtin)) then
      xtout = xpath_cast(stringify(xtin))
    else
      xtout = xpath_cast(stringify(contextNode))
    endif

  end function xpath_string

  function xpath_concat(xt1, xt2) result(xtout)
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xtout

    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as first argument of concat")
    call xpath_assert(xt2%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as second argument of concat")
    
    xtout%type = XMLF90_XPATH_TYPE_STRING
    xtout%string = xt1%string + xt2%string

  end function xpath_concat

  function xpath_starts_with(xt1, xt2) result(xtout)
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xtout

    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as first argument of starts-with")
    call xpath_assert(xt2%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as second argument of starts-with")

    xtout%type = XMLF90_XPATH_TYPE_BOOLEAN
    xtout%boolean =  (index(xt1%string, xt2%string) == 1)

  end function xpath_starts_with

  function xpath_contains(xt1, xt2) result(xtout)
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xtout

    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as first argument of contains")
    call xpath_assert(xt2%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as second argument of contains")

    xtout%type = XMLF90_XPATH_TYPE_BOOLEAN
    xtout%boolean = (index(xt1%string, xt2%string) > 0)

  end function xpath_contains

  function xpath_substring_before(xt1, xt2) result(xtout)
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xtout
    integer :: n
    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as first argument of substring-before")
    call xpath_assert(xt2%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as second argument of substring-before")

    xtout%type = XMLF90_XPATH_TYPE_STRING
    n = index(xt1%string, xt2%string)
    if (n==0) then
      xtout%string = ''
    else
      xtout%string = substr(xt1%string, 1, n)
    endif

  end function xpath_substring_before
   
  function xpath_substring_after(xt1, xt2) result(xtout)
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xtout
    integer :: n
    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as first argument of substring-after")
    call xpath_assert(xt2%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as second argument of substring-after")

    xtout%type = XMLF90_XPATH_TYPE_STRING
    n = index(xt1%string, xt2%string, .true.)
    if (n==0) then
      xtout%string = ''
    else
      xtout%string = substr(xt1%string, n+1, len(xt1%string))
    endif

  end function xpath_substring_after

  function xpath_substring(xt1, xt2, xt3) result(xtout)
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type), optional, intent(in) :: xt3
    type(xpath_type) :: xtout

    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried to pass non-string as first argument of substring-")
    call xpath_assert(xt2%type == XMLF90_XPATH_TYPE_NUMBER, &
      "Tried to pass non-number as second argument of substring")
    if (present(xt3)) &
      call xpath_assert(xt3%type == XMLF90_XPATH_TYPE_NUMBER, &
      "Tried to pass non-number as third argument of substring")

    xtout%type = XMLF90_XPATH_TYPE_STRING
    if (present(xt3)) then
      xtout%string = &
        substr(xt1%string, &
        nint(xt2%number), nint(xt3%number)-nint(xt2%number)+1)
    else
      xtout%string = &
        substr(xt1%string, &
        nint(xt2%number), len(xt1%string))
    endif

  end function xpath_substring

  function xpath_string_length(xt1) result(xtout)
    type(xpath_type), optional, intent(in) :: xt1
    type(xpath_type) :: xtout
    type(string) :: context_string

    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    if (present(xt1)) then
      call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_STRING, &
        "Tried to pass non-string to string-length")
      xtout%number = len(xt1%string)
    else
      context_string = stringify(contextNode)
      xtout%number = len(context_string)
    endif
  end function xpath_string_length

  function xpath_normalize_space(xt1) result(xtout)
    type(xpath_type), optional, intent(in) :: xt1
    type(xpath_type) :: xtout
    type(string) :: abnormal_string

    xtout%type = XMLF90_XPATH_TYPE_STRING
    if (present(xt1)) then
      call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_STRING, &
        "Tried to pass non-string to normalize-space")
      abnormal_string = xt1 % string
    else
      abnormal_string = stringify(contextNode)
    endif

    !TOHW finish
  end function xpath_normalize_space

  function xpath_translate(xt1, xt2, xt3) result(xtout)
    type(xpath_type), intent(in) :: xt1, xt2, xt3
    type(xpath_type) :: xtout

    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_STRING, &
        "Tried to pass non-string to first argument of translate")
    call xpath_assert(xt2%type == XMLF90_XPATH_TYPE_STRING, &
        "Tried to pass non-string to second argument of translate")
    call xpath_assert(xt3%type == XMLF90_XPATH_TYPE_STRING, &
        "Tried to pass non-string to third argument of translate")
    
    xtout%type = XMLF90_XPATH_TYPE_STRING
    
    !TOHW finish
  end function xpath_translate

end module m_xpath_string
    
