program dom_example

  use m_common_array_str
  use FoX_dom

  type(Node), pointer :: myDoc, np, np2
  type(NodeList) :: interest

  myDoc => parsefile('test.xml')

  np => createElement(myDoc, 'a')

  np2 => getDocumentElement(myDoc)

  np => appendChild(np2, np)

  call setAttribute(np, "a", "b")

!  call dumpTree(myDoc)

!  interest = getElementsByTagName(myDoc, 'john1')

!  print*, interest%length
  
!  call destroyNodeList(interest)

  call destroyDocument(myDoc)

end program dom_example
