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

  call setAttributeNS(np, "http://www.xml-cml.org/schema", "a:b", "c")

  np => getNextSibling(getFirstChild(np2))

  call removeAttribute(np, "b")

  print*,getNodeType(np)
  np => item(getChildNodes(np), 1)

  print*,getNodeType(getAttributeNode(np, "e"))
  np => getAttributeNode(np, "e")
!  np => removeAttributeNode(np, getAttributeNode(np, "e"))
  np => removeAttributeNode(getOwnerElement(np), np)

!  call dumpTree(myDoc)

!  interest = getElementsByTagName(myDoc, 'john1')

  call destroyDocument(myDoc)

end program dom_example
