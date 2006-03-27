module m_xpath_number

! Module containing all number-related functions of xpath

  use m_xpath_types, only : xpath_type, contextNode
  use m_xpath_types, only : XMLF90_XPATH_TYPE_NODESET, XMLF90_XPATH_TYPE_BOOLEAN
  use m_xpath_types, only : XMLF90_XPATH_TYPE_NUMBER, XMLF90_XPATH_TYPE_STRING
  use m_xpath_types, only : real, xpath_cast

  use m_xpath_error, only : xpath_assert

  use xmlf90_dom, only: getLength, item

  implicit none
  private

  integer, parameter :: dp = selected_real_kind(14,100) 

  public :: xpath_add
  public :: xpath_subtract
  public :: xpath_multiply
  public :: xpath_divide
  public :: xpath_number
  public :: xpath_floor
  public :: xpath_ceiling
  public :: xpath_round
  public :: xpath_negate
  public :: xpath_sum

contains

  function xpath_number(xtin) result(xtout)
    type(xpath_type), optional, intent(in) :: xtin
    type(xpath_type) :: xtout

    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    if (present(xtin)) then
      xtout%number = real(xtin)
    else
      xtout%number = real(contextNode)
    endif
  end function xpath_number

  function xpath_add(xt1, xt2) result(xt12) 
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xt12
    
    xt12%type = XMLF90_XPATH_TYPE_NUMBER
    xt12%number = private_maths(real(xt1), real(xt2), '+')
    
  end function xpath_add

    function xpath_subtract(xt1, xt2) result(xt12) 
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xt12
    
    xt12%type = XMLF90_XPATH_TYPE_NUMBER
    xt12%number = private_maths(real(xt1), real(xt2), '-')
    
  end function xpath_subtract

  function xpath_multiply(xt1, xt2) result(xt12) 
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xt12
    
    xt12%type = XMLF90_XPATH_TYPE_NUMBER
    xt12%number = private_maths(real(xt1), real(xt2), '*')
      
  end function xpath_multiply

  function xpath_divide(xt1, xt2) result(xt12) 
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xt12
    
    xt12%type = XMLF90_XPATH_TYPE_NUMBER
    xt12%number = private_maths(real(xt1), real(xt2), '/')
    
  end function xpath_divide

  function xpath_mod(xt1, xt2) result(xt12) 
    type(xpath_type), intent(in) :: xt1, xt2
    type(xpath_type) :: xt12
    
    xt12%type = XMLF90_XPATH_TYPE_NUMBER
    xt12%number = private_maths(real(xt1), real(xt2), 'mod')
    
  end function xpath_mod
    
  function private_maths(n1, n2, op) result(n12)
    real(dp), intent(in) :: n1, n2
    character(len=*), intent(in) :: op
    real(dp) :: n12
    
    select case(op)
    case ('+')
      n12 = n1 + n2
    case ('-')
      n12 = n1 - n2
    case ('*')
      n12 = n1 * n2
    case ('/')
      n12 = n1 / n2
    case ('mod')
      n12 = mod(n1, n2)
    end select

  end function private_maths

  function xpath_floor(xt1) result(xtout)
    type(xpath_type), intent(in) :: xt1
    type(xpath_type) :: xtout
    
    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_NUMBER, &
      "Tried to use non-number as argument to floor")
    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    xtout%number = floor(xt1%number)
  end function xpath_floor
  
  function xpath_ceiling(xt1) result(xtout)
    type(xpath_type), intent(in) :: xt1
    type(xpath_type) :: xtout
    
    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_NUMBER, &
      "Tried to use non-number as argument to ceiling")
    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    xtout%number = ceiling(xt1%number)
  end function xpath_ceiling

  function xpath_round(xt1) result(xtout)
    type(xpath_type), intent(in) :: xt1
    type(xpath_type) :: xtout
    
    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_NUMBER, &
      "Tried to use non-number as argument to round")
    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    xtout%number = nint(xt1%number)
  end function xpath_round

  function xpath_negate(xt1) result(xtout)
    type(xpath_type), intent(in) :: xt1
    type(xpath_type) :: xtout
    
    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_NUMBER, &
      "Tried to negate a non-number")
    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    xtout%number = -xt1%number
  end function xpath_negate

  function xpath_sum(xt1) result(xtout)
    type(xpath_type), intent(in) :: xt1
    type(xpath_type) :: xtout

    integer :: i
    
    call xpath_assert(xt1%type == XMLF90_XPATH_TYPE_NODESET, &
      "Tried to use a non-nodeset as argument to sum")
    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    xtout%number = 0
    do i = 1, getLength(xt1%nodeset)
      xtout%number = xtout%number + real(xpath_cast(item(xt1%nodeset,i)))
    enddo
  end function xpath_sum

end module m_xpath_number
