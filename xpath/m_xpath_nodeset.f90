module m_xpath_nodeset

  use m_xpath_types, only : xpath_type, contextNode
  use m_xpath_types, only : XMLF90_XPATH_TYPE_NODESET, XMLF90_XPATH_TYPE_BOOLEAN
  use m_xpath_types, only : XMLF90_XPATH_TYPE_NUMBER, XMLF90_XPATH_TYPE_STRING
  use m_xpath_types, only : real, xpath_cast

  use m_xpath_error, only : xpath_assert

  use xmlf90_dom, only : fnode, getLength, item

contains

  function xpath_last() result(xtout)
    type(xpath_type) :: xtout

    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    xtout%number = getLength(contextNode%nodeset)
  end function xpath_last

  function xpath_position() result(xtout)
    type (xpath_type) :: xtout

    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    !TOHW finish
  end function xpath_position
  
  function xpath_count(xtin) result(xtout)
    type(xpath_type), intent(in) :: xtin
    type(xpath_type) :: xtout

    call xpath_assert(xtin%type==XMLF90_XPATH_TYPE_NODESET, &
      "Tried to pass non-nodeset as argument to count")
    xtout%type = XMLF90_XPATH_TYPE_NUMBER
    xtout%number = getLength(xtin%nodeset)
  end function xpath_count

  function xpath_id(xtin) result(xtout)
    type(xpath_type), intent(in) :: xtin
    type(xpath_type) :: xtout

    xtout%type = XMLF90_XPATH_TYPE_NODESET
    !TOHW finish
  end function xpath_id

  function xpath_local_name(xtin) result(xtout)
    type(xpath_type), optional, intent(in) :: xtin
    type(xpath_type) :: xtout
    type(fnode) :: node

    if (present(xtin)) then
      call xpath_assert(xtin%type==XMLF90_XPATH_TYPE_NODESET, &
        "Tried to pass non-nodeset as argument to local-name")
      node = item(xtin%nodeset, 1)
    else
      node = item(contextNode%nodeset, 1)
    endif
    xtout%type = XMLF90_XPATH_TYPE_STRING

    !TOHW finish
  end function xpath_local_name
    
  function xpath_namespace_uri(xtin) result(xtout)
    type(xpath_type), optional, intent(in) :: xtin
    type(xpath_type) :: xtout
    type(fnode) :: node

    if (present(xtin)) then
      call xpath_assert(xtin%type==XMLF90_XPATH_TYPE_NODESET, &
        "Tried to pass non-nodeset as argument to namespace-uri")
      node = item(xtin%nodeset, 1)
    else
      node = item(contextNode%nodeset, 1)
    endif
    xtout%type = XMLF90_XPATH_TYPE_STRING

    !TOHW finish
  end function xpath_namespace_uri
    
  function xpath_name(xtin) result(xtout)
    type(xpath_type), optional, intent(in) :: xtin
    type(xpath_type) :: xtout
    type(fnode) :: node

    if (present(xtin)) then
      call xpath_assert(xtin%type==XMLF90_XPATH_TYPE_NODESET, &
        "Tried to pass non-nodeset as argument to name")
      node = item(xtin%nodeset, 1)
    else
      node = item(contextNode%nodeset, 1)
    endif
    xtout%type = XMLF90_XPATH_TYPE_STRING

    !TOHW finish
  end function xpath_name
  

end module m_xpath_nodeset
