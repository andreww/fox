program dom_example

  use m_common_array_str
  use FoX_dom

  type(Node), pointer :: myDoc, np, np2, dummy
  type(NodeList) :: interest

  myDoc => parsefile('test.xml')

  np => createElement(myDoc, 'a')

  dummy => createDocumentFragment(myDoc)
  dummy => createDocumentFragment(myDoc)
  dummy => createDocumentFragment(myDoc)

  np2 => getDocumentElement(myDoc)

  np => appendChild(np2, np)

  call setAttribute(np, "a", "b")

  call setAttributeNS(np, "http://www.xml-cml.org/schema", "a:b", "c")

  np => getNextSibling(getFirstChild(np2))

  call removeAttribute(np, "b")

  print*,getNodeType(np)
  np => item(getChildNodes(np), 1)

  call setNodeValue(getAttributeNode(np, "e"), "fg")

  np => removeAttributeNode(np, getAttributeNode(np, "e"))

  np => removeChild(np2, getFirstChild(np2))

!  dummy => appendChild(np, createTextNode("lalal")
!  dummy => appendChild(np2, np)

!  call dumpTree(myDoc)

  interest = getElementsByTagName(myDoc, 'a')

  call destroyDocument(myDoc)

end program dom_example
