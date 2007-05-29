program dom_example

  use m_common_array_str
  use FoX_dom

  type(Node), pointer :: myDoc
  type(NodeList) :: interest

  myDoc => parsefile('test.xml')

  call dumpTree(myDoc)

!  interest = getElementsByTagName(myDoc, 'john1')

!  print*, interest%length
  
!  call destroyNodeList(interest)

  call destroyDocument(myDoc)

end program dom_example
