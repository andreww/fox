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

  public :: getIsId

')`'dnl
dnl
TOHW_m_dom_contents(`
  
  ! function getName(attribute) result(c) See m_dom_common

! NB All functions manipulating attributes play with the nodelist
! directly rather than through helper functions.
! This is so that getValue_length can be pure,  and the nodeList
! can be explicitly kept up to dat.

TOHW_m_dom_get(logical, specified, np%elExtras%specified, (ATTRIBUTE_NODE))

TOHW_m_dom_set(logical, specified, np%elExtras%specified, (ATTRIBUTE_NODE))

TOHW_m_dom_get(logical, isId, np%elExtras%isId, (ATTRIBUTE_NODE))

TOHW_m_dom_set(logical, isId, np%elExtras%isId, (ATTRIBUTE_NODE))

TOHW_m_dom_get(Node, ownerElement, np%elExtras%ownerElement, (ATTRIBUTE_NODE))

  TOHW_function(getValue_DOM, (arg), c)
    type(Node), pointer :: arg
    character(len=getTextContent_len(arg, associated(arg))) :: c 

    integer :: i, n

    if (.not.associated(arg)) then
      TOHW_m_dom_throw_error(FoX_NODE_IS_NULL)
    endif

    if (getNodeType(arg)/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    c = getTextContent(arg)

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

')`'dnl
