program dom_example

  use m_common_array_str
  use FoX_dom
  implicit none

  type(Node), pointer :: myDoc, myOtherDoc, np, np2, dummy
  type(NodeList) :: interest
  integer :: i

  myDoc => parsefile('test.xml')
  myOtherDoc => parsefile('test.xml')
  print*,"LEN", getLength(getChildNodes(getDocumentElement(myDoc)))
  np => createElement(myDoc, 'a')

  dummy => createDocumentFragment(myDoc)
  dummy => createDocumentFragment(myDoc)

  np2 => getDocumentElement(myDoc)

  np => importNode(myDoc, getDocumentElement(myOtherDoc), .true.)

  np => appendChild(np2, np)
  call serialize(myDoc, 'myDoc.xml')
  call serialize(myOtherDoc, 'myOtherDoc.xml')

  np => appendChild(np2, np)

  call setAttribute(np, "a", "b")

  call setAttributeNS(np, "http://www.xml-cml.org/schema", "a:b", "c")

  np => getNextSibling(getFirstChild(np2))

  call removeAttribute(np, "b")

  print*,getNodeType(np)
  np => item(getChildNodes(np), 1)
  print*, getNodeType(np)
  do i = 1, getLength(getAttributes(np))
    print*, getName(item(getAttributes(np), i-1))
  enddo
  call setNodeValue(getAttributeNode(np, "e"), "fg")

  np => removeAttributeNode(np, getAttributeNode(np, "e"))
  dummy => cloneNode(np, .true.)
  np => removeChild(np2, getFirstChild(np2))
  np => createDocumentFragment(myDoc)
  dummy => createDocumentFragment(myDoc)
  np => appendChild(dummy, createTextNode(myDoc, "lalal"))
  np => appendChild(dummy, createTextNode(myDoc, " lalal"))
  print*,getLength(getChildNodes(np2))
  dummy => insertBefore(np2, dummy, null())
  print*,"LEN", getLength(getChildNodes(np2))
  do i = 1, getLength(getChildNodes(np2))
    print*, "NT", getNodeType(item(getChildNodes(np2), i-1))
  enddo

  dummy => appendChild(np2, cloneNode(np2, .true.))

  call serialize(myDoc, "out321.xml")
  call dumpTree(myDoc)
!  call dumpTree(myDoc)

  interest = getElementsByTagName(myDoc, 'a')

  call destroyDocument(myDoc)
  call destroyDocument(myOtherDoc)
end program dom_example
