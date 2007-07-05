TOHW_m_dom_imports(`

  use m_common_array_str, only: str_vs, vs_str_alloc

')`'dnl
dnl
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


  TOHW_function(getSpecified, (attribute), p)
    type(Node), pointer :: attribute
    logical :: p

    if (attribute%nodeType/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    p = attribute%specified
  end function getSpecified

  TOHW_subroutine(setSpecified, (attribute, p))
    type(Node), pointer :: attribute
    logical, intent(in) :: p

    if (attribute%nodeType/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    attribute%specified = p
  end subroutine setSpecified
    
  pure function getValue_length(attribute) result(n)
    type(Node), intent(in) :: attribute
    integer :: n

    integer :: i

    n = 0
    do i = 1, attribute%childNodes%length
      if (attribute%childNodes%nodes(i)%this%nodeType==TEXT_NODE) then
        n = n + size(attribute%childNodes%nodes(i)%this%nodeValue)
      else
    ! FIXME get entity length
      endif
    enddo

  end function getValue_length

  TOHW_function(getValue_DOM, (attribute), c)
    type(Node), intent(in) :: attribute
    character(len=getValue_length(attribute)) :: c 

    integer :: i, n

    if (attribute%nodeType/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    n = 1
    print*, "we have ",  attribute%childNodes%length, "children."
    do i = 1, attribute%childNodes%length
      if (attribute%childNodes%nodes(i)%this%nodeType==TEXT_NODE) then
        c(n:n+size(attribute%childNodes%nodes(i)%this%nodeValue)-1) = &
          str_vs(attribute%childNodes%nodes(i)%this%nodeValue)
      else
    ! FIXME get entity value
      endif
    enddo

  end function getValue_DOM


  TOHW_subroutine(setValue, (attribute, value))
    type(Node), pointer :: attribute
    character(len=*), intent(in) :: value

    type(Node), pointer :: np
    integer :: i

    if (attribute%nodeType/=ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    elseif (attribute%readonly) then
      TOHW_m_dom_throw_error(NO_MODIFICATION_ALLOWED_ERR)
    elseif (.not.checkChars(value, getXmlVersionEnum(getOwnerDocument(attribute)))) then
      TOHW_m_dom_throw_error(FoX_INVALID_CHARACTER)
    endif

    do i = 1, attribute%childNodes%length
      call destroyNode(attribute%childNodes%nodes(i)%this)
    enddo
    deallocate(attribute%childNodes%nodes)
    allocate(attribute%childNodes%nodes(0))
    attribute%childNodes%length = 0
    attribute%firstChild => null()
    attribute%lastChild => null()
    np => createTextNode(attribute%ownerDocument, value)
    np => appendChild(attribute, np)

  end subroutine setValue


  TOHW_function(getOwnerElement, (attribute), np)
    type(Node), intent(in) :: attribute
    type(Node), pointer :: np

    if (attribute%nodeType /= ATTRIBUTE_NODE) then
      TOHW_m_dom_throw_error(FoX_INVALID_NODE)
    endif

    np => attribute%ownerElement

  end function getOwnerElement

')`'dnl
