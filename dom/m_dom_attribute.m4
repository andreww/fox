TOHW_m_dom_publics(`

  !public :: getName
  public :: getSpecified
  public :: setSpecified
  interface getValue
    module procedure getValue_DOM
  end interface	 
  public :: getValue
  public :: setValue
  public :: getOwnerElement

')`'dnl
dnl
TOHW_m_dom_contents(`
  
  ! function getName(attribute) result(c) See m_dom_common

! NB All functions manipulating attributes play with the nodelist
! directly rather than through helper functions.
! This is so that getValue_length can be pure,  and the nodeList
! can be explicitly kept up to dat.


  TOHW_function(getSpecified, (arg), p)
    type(Node), pointer :: arg
    logical :: p

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    p = arg%elExtras%specified
  end function getSpecified

  TOHW_subroutine(setSpecified, (arg, p))
    type(Node), pointer :: arg
    logical, intent(in) :: p

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    arg%elExtras%specified = p
  end subroutine setSpecified
    
  pure function getValue_len(arg, p) result(n)
    type(Node), intent(in) :: arg
    logical, intent(in) :: p
    integer :: n

    integer :: i

    n = 0 
    if (.not.p) return

    do i = 1, arg%childNodes%length
      n = n + size(arg%childNodes%nodes(i)%this%nodeValue)
    enddo

  end function getValue_len

  TOHW_function(getValue_DOM, (arg), c)
    type(Node), pointer :: arg
    character(len=getValue_len(arg, associated(arg))) :: c 

    integer :: i, n

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    n = 1
    do i = 1, arg%childNodes%length
      c(n:n+size(arg%childNodes%nodes(i)%this%nodeValue)-1) = &
        str_vs(arg%childNodes%nodes(i)%this%nodeValue)
    enddo

  end function getValue_DOM

  TOHW_subroutine(setValue, (arg, value))
    type(Node), pointer :: arg
    character(len=*), intent(in) :: value

    type(Node), pointer :: np
    integer :: i

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (arg%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (.not.checkChars(value, getXmlVersionEnum(getOwnerDocument(arg)))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    endif

    do i = 1, getLength(getChildNodes(arg))
      call destroyNode(arg%childNodes%nodes(i)%this)
    enddo
    deallocate(arg%childNodes%nodes)
    allocate(arg%childNodes%nodes(0))
    arg%childNodes%length = 0
    arg%firstChild => null()
    arg%lastChild => null()
    np => createTextNode(getOwnerDocument(arg), value)
    np => appendChild(arg, np)

  end subroutine setValue

  TOHW_function(getOwnerElement, (arg), np)
    type(Node), pointer :: arg
    type(Node), pointer :: np

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg) /= ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    np => arg%ownerElement

  end function getOwnerElement

  TOHW_function(getIsId, (arg), p)
    type(Node), pointer :: arg
    logical :: p

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg) /= ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    p = arg%elExtras%isId

  end function getIsId

  TOHW_subroutine(setIsId, (arg, p))
    type(Node), pointer :: arg
    logical, intent(in) :: p

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg) /= ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    arg%elExtras%isId = p

  end subroutine setIsId

')`'dnl
