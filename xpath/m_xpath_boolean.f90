module m_xpath_boolean

  use m_xpath_types, only : xpath_type, contextNode
  use m_xpath_types, only : XMLF90_XPATH_TYPE_NODESET, XMLF90_XPATH_TYPE_BOOLEAN
  use m_xpath_types, only : XMLF90_XPATH_TYPE_NUMBER, XMLF90_XPATH_TYPE_STRING
  use m_xpath_types, only : logical

  use m_xpath_error, only : xpath_assert

  use m_strings, only : string

  implicit none
  private

  public :: xpath_boolean
  public :: xpath_not
  public :: xpath_true
  public :: xpath_false
  public :: xpath_lang

contains

  function xpath_boolean(xtin) result(xtout)
    type(xpath_type), intent(in) :: xtin
    type(xpath_type) :: xtout
    xtout%type = XMLF90_XPATH_TYPE_BOOLEAN
    xtout%boolean = logical(xtin)
  end function xpath_boolean

  function xpath_not(xtin) result(xtout)
    type(xpath_type), intent(in) :: xtin
    type(xpath_type) :: xtout
    call xpath_assert(xtin%type == XMLF90_XPATH_TYPE_BOOLEAN, &
      "Tried use non-boolean for argument of not")
    xtout%type = XMLF90_XPATH_TYPE_BOOLEAN
    xtout%boolean = .not.xtin%boolean
  end function xpath_not

  function xpath_true() result(xtout)
    type(xpath_type) :: xtout
    xtout%type = XMLF90_XPATH_TYPE_BOOLEAN
    xtout%boolean = .true.
  end function xpath_true
      
  function xpath_false() result(xtout)
    type(xpath_type) :: xtout
    xtout%type = XMLF90_XPATH_TYPE_BOOLEAN
    xtout%boolean = .false.
  end function xpath_false

  function xpath_lang(lang) result(xtout)
    type(xpath_type), intent(in) :: lang
    type(xpath_type) :: xtout
    call xpath_assert(lang%type == XMLF90_XPATH_TYPE_STRING, &
      "Tried use non-string for argument of lang")
    xtout%type = XMLF90_XPATH_TYPE_BOOLEAN
    !TOHW finish
    continue
  end function xpath_lang

end module m_xpath_boolean
