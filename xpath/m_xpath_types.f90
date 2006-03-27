module m_xpath_types

  use xmlf90_dom, only: fNode, fNodeList, getLength, item, getNodeValue
  use xmlf90_dom, only: DOCUMENT_NODE, ELEMENT_NODE, TEXT_NODE
  !use xmlf90_dom, only: ATTRIBUTE_NODE, NAMESPACE_NODE, COMMENT_NODE
  use xmlf90_dom, only: ATTRIBUTE_NODE, COMMENT_NODE
  use xmlf90_dom, only: PROCESSING_INSTRUCTION_NODE

  use m_ieee, only: generate_nan, is_nan, is_inf

  use m_strings, only: string, stringify, len, assignment(=)

  implicit none
  private

  integer, parameter :: dp = selected_real_kind(14,100) 

  integer, parameter, public :: XMLF90_XPATH_TYPE_INVALID = 0
  integer, parameter, public :: XMLF90_XPATH_TYPE_NODESET = 1
  integer, parameter, public :: XMLF90_XPATH_TYPE_BOOLEAN = 2
  integer, parameter, public :: XMLF90_XPATH_TYPE_NUMBER  = 3
  integer, parameter, public :: XMLF90_XPATH_TYPE_STRING  = 4

  type xpath_type
    integer                  :: type = XMLF90_XPATH_TYPE_INVALID
    type(fNodeList), pointer :: nodeset
    logical                  :: boolean = .false.
    real(dp)                 :: number = 0.0_dp
    type(string)             :: string
  end type xpath_type

  ! XML whitespace
  character, parameter :: HT = achar(9)
  character, parameter :: LF = achar(10)
  character, parameter :: CR = achar(13)
  character, parameter :: SP = achar(32)
  character(len=*), parameter :: WHITESPACE = SP//HT//LF//CR
  ! XML character classes stripped down to 7 bit
  character(len=*), parameter :: LETTER = &
    'abcdefghijklmnopqrstuvwxyz' &
    // 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'!//the rest of Unicode!
  character(len=*), parameter :: DIGIT = &
     '0123456789'
  character(len=*), parameter :: NCNAMECHAR = &
    LETTER//DIGIT//'.'//'-'//'_'!//CombiningChar//Extender


  interface real
    module procedure number_from_xpath_type
  end interface real

  interface stringify
    module procedure string_from_xpath_type
  end interface stringify

  interface logical
    module procedure boolean_from_xpath_type
  end interface logical

  interface xpath_cast
    module procedure xpath_type_from_string, xpath_type_from_number
    module procedure xpath_type_from_boolean, xpath_type_from_nodeset
    module procedure xpath_type_from_node
  end interface

  type(xpath_type), save :: currentLocation
  type(xpath_type), save :: contextNode

  type(xpath_type), save :: rootnode

  public :: xpath_type
  public :: print_xpath
  public :: addstep, xpath, locationPath
  public :: currentLocation, contextNode, rootnode

  public :: real
  public :: stringify
  public :: logical

  public :: xpath_cast

  public :: string_value


  public :: WHITESPACE
  public :: DIGIT
  public :: LETTER
  public :: NCNAMECHAR

contains


  subroutine print_xpath(t)
    type(xpath_type), intent(in) :: t
    
    select case (t%type)
    case (XMLF90_XPATH_TYPE_NODESET)
      print*,"Nodeset: "
    case (XMLF90_XPATH_TYPE_BOOLEAN)
      print*,"Logical: ", t%boolean
    case (XMLF90_XPATH_TYPE_NUMBER)
      print*,"Number: ", t%number
    case (XMLF90_XPATH_TYPE_STRING)
      print*,"String: ", stringify(t%string)
    case default
      print*,"wtf?"
    end select
    
  end subroutine print_xpath


  function xpath_type_from_string(str) result(xt)
    type(string), intent(in) :: str
    type(xpath_type) :: xt
    
    xt%type = XMLF90_XPATH_TYPE_STRING
    xt%string = str
  end function xpath_type_from_string
    
  function xpath_type_from_number(number) result(xt)
    real(dp), intent(in) :: number
    type(xpath_type) :: xt
    
    xt%type = XMLF90_XPATH_TYPE_NUMBER
    xt%number = number
  end function xpath_type_from_number

  function xpath_type_from_boolean(boolean) result(xt)
    logical, intent(in) :: boolean
    type(xpath_type) :: xt
    
    xt%type = XMLF90_XPATH_TYPE_BOOLEAN
    xt%boolean = boolean
  end function xpath_type_from_boolean

  function xpath_type_from_nodeset(nodeset) result(xt)
    type(fNodeList), pointer :: nodeset
    type(xpath_type) :: xt
    
    xt%type = XMLF90_XPATH_TYPE_NODESET
    xt%nodeset => nodeset
  end function xpath_type_from_nodeset

  function xpath_type_from_node(node) result(xt)
    type(fNode), pointer :: node
    type(xpath_type) :: xt

    type(fNodeList), pointer :: nodeset

    call append(nodeset, node)
    
    xt%type = XMLF90_XPATH_TYPE_NODESET
    xt%nodeset => nodeset
  end function xpath_type_from_node


  recursive function string_from_xpath_type(xtin) result(str)
    type(xpath_type), intent(in) :: xtin
    type(string) :: str

    select case (xtin%type)
    case(XMLF90_XPATH_TYPE_NODESET)
      str = string_value(item(xtin%nodeset,1)) !NB This should be the first *in document order*
    case(XMLF90_XPATH_TYPE_BOOLEAN)
      if (xtin%boolean) then
        str = 'true'
      else
        str = 'false'
      endif
    case(XMLF90_XPATH_TYPE_NUMBER)
      str = trim(n_to_str(xtin%number))
    case(XMLF90_XPATH_TYPE_STRING)
      str = xtin%string
    end select

    contains
      function n_to_str(n) result(str)
        real(dp) :: n
        character(100) :: str

        if (is_nan(n)) then
          str='NaN'
        elseif (is_inf(n)) then
          if (n > 0.0) then
            str='Infinity'
          else
            str='-Infinity'
          endif
        elseif (n==0.0) then
          str='0'
        else
          write(str,'(f100.50)') n
        endif
      end function n_to_str

    end function string_from_xpath_type

  recursive function string_value(node) result(str)
    use xmlf90_dom, only: fNodeList, getLength, item
    use m_strings, only: concatenate
    type(fnode), pointer :: node
    type(string) :: str

    type(string), allocatable :: texts(:)
    type(fNodeList), pointer :: textnodes
    integer :: i, n

    select case(node%nodeType)
    case (DOCUMENT_NODE)
      textnodes = get_text_descendants(node)
      n = getLength(textnodes)
      allocate(texts(n))
      do i = 1, n
        texts(i) = getNodevalue(item(textnodes,i)) 
      enddo
      deallocate(texts)
      str = concatenate(texts)
    case (ELEMENT_NODE)
      textnodes = get_text_descendants(node)
      n = getLength(textnodes)
      allocate(texts(n))
      do i = 1, n
        texts(i) = getNodevalue(item(textnodes,i))
      enddo
      deallocate(texts)
      str = concatenate(texts)
    case (TEXT_NODE)
      str = getNodeValue(node)
    case (ATTRIBUTE_NODE)
      str = getNodeValue(node)
    !case (NAMESPACE_NODE)
    !  str = getNodeValue(node)
    case (PROCESSING_INSTRUCTION_NODE)
      str = getNodeValue(node)
    case (COMMENT_NODE)
      str = getNodeValue(node)
    case default
      call xpath_error('Encountered unexpected nodeType in string_value')
    end select

  end function string_value

  recursive function get_text_descendants(node) result(nodemap)
    use xmlf90_dom, only: fnodeList
    use xmlf90_dom, only: append, getChildNodes
    type(fnode), pointer :: node
    type(fNodeList), pointer :: nodemap
    
    type(fnode), pointer :: child, dummy
    type(fNodeList), pointer :: children, othertextnodes
    integer :: i, j

    children = getChildNodes(node)
    do i = 1, getLength(children)
      child = item(children ,i)
      if (child%nodeType == TEXT_NODE) then
        call append(nodemap, child)
      else
        othertextnodes = get_text_descendants(child)
        do j = 1, getLength(othertextnodes)
          call append(nodemap, item(othertextnodes, i))
        enddo
        call destroyNamedNodeMap(othertextnodes)
      endif
    enddo

  end function get_text_descendants



  function number_from_xpath_type(xtin) result(number)  
    type(xpath_type), intent(in) :: xtin
    real(dp) :: number
    type(string) :: s_nodeset

    select case (xtin%type)
    case(XMLF90_XPATH_TYPE_NODESET)
      s_nodeset = string_value(item(xtin%nodeset,1))
      number = str_to_n(s_nodeset%s)
    case(XMLF90_XPATH_TYPE_BOOLEAN)
      number = merge(1, 0, xtin%boolean)
    case(XMLF90_XPATH_TYPE_NUMBER)
      number = xtin%number
    case(XMLF90_XPATH_TYPE_STRING)
      number = str_to_n(xtin%string%s)
      end select
    contains
    
    function str_to_n(c) result(r)
      character :: c(:)
      real(dp) :: r
      character(len=size(c)) :: s
      integer :: i
      
      i = 0
      s = ''
      do while (i<=size(c))
        if (scan(c(i), WHITESPACE)==0) exit
        i = i+1
      enddo
      if (i<size(c)) then
        if (c(i) == '-') then
          s = trim(s) // '-'
          i = i + 1
        endif
      endif
      do while (i<=size(c))
        if (scan(c(i), DIGIT)==0) exit
        s = c(i)
        i = i+1
      enddo
      if (i<=size(c)) then
        if (c(i) == '.') then
          s = trim(s) // '.'
          i = i + 1
        endif
      endif
      do while (i<=size(c))
        if (scan(c(i), DIGIT)==0) exit
        s = c(i)
        i = i+1
      enddo
      do while (i<=size(c))
        if (scan(c(i), WHITESPACE)/=0) then
          s = ''
        endif
      enddo
      
      if (s=='') then
        r = generate_nan()
      else
        read(s, '(f100.50)') r
      endif
      
    end function str_to_n

  end function number_from_xpath_type

  function boolean_from_xpath_type(xtin) result(boolean)
    type(xpath_type), intent(in) :: xtin
    logical :: boolean
    
    select case (xtin%type)
    case (XMLF90_XPATH_TYPE_NODESET)
      boolean = (getlength(xtin%nodeset) > 0)
    case (XMLF90_XPATH_TYPE_BOOLEAN)
      boolean = xtin%boolean
    case (XMLF90_XPATH_TYPE_NUMBER)
      boolean = (xtin%number == 0.0 .or. is_nan(xtin%number))
    case (XMLF90_XPATH_TYPE_STRING)
      boolean = (len(xtin%string) > 0)
    end select

  end function boolean_from_xpath_type


  function xpath() result(x)
    type(xpath_type) :: x
  end function xpath

  function locationPath(path) result(n)
    type(xpath_type) :: n
    type(xpath_type), intent(in) :: path

    n%type=XMLF90_XPATH_TYPE_NODESET
    ! parse path somehow.

  end function locationPath

  function addStep(loc1, step) result(n)
    type(xpath_type) :: n
    type(xpath_type), intent(in) :: loc1, step

  end function addStep

end module m_xpath_types
