program dom_example

  use m_common_array_str
  use FoX_dom
  implicit none

  type(DOMImplementation), pointer :: imp
  type(Node), pointer :: myDoc, myOtherDoc, np, np2, dummy
  type(NodeList) :: interest
  integer :: i

  myDoc => parsefile('test.xml', "entities")
  call dumpTree(myDoc)
  call serialize(myDoc, 'myDoc.xml')
!!$stop
!!$  myOtherDoc => parsefile('test.xml')
!!$  np => createElement(myDoc, 'a')
!!$
!!$  imp => getImplementation(myDoc)
!!$
!!$  dummy => createDocumentFragment(myDoc)
!!$  dummy => createDocumentFragment(myDoc)
!!$
!!$  np2 => getDocumentElement(myDoc)
!!$  np => importNode(myDoc, getDocumentElement(myOtherDoc), .true.)
!!$
!!$  np => appendChild(np2, np)
!!$  call serialize(myOtherDoc, 'myOtherDoc.xml')
!!$  np => appendChild(np2, np)
!!$
!!$  call setAttribute(np, "a", "b")
!!$
!!$  np => getNextSibling(getFirstChild(np2))
!!$
!!$  call removeAttribute(np, "b")
!!$
!!$  np => item(getChildNodes(np), 1)
!!$  call setNodeValue(getAttributeNode(np, "e"), "fg")
!!$
!!$  np => removeAttributeNode(np, getAttributeNode(np, "e"))
!!$  dummy => cloneNode(np, .true.)
!!$  np => removeChild(np2, getFirstChild(np2))
!!$  np => createDocumentFragment(myDoc)
!!$  dummy => createDocumentFragment(myDoc)
!!$  np => appendChild(dummy, createTextNode(myDoc, "lalal"))
!!$  np => appendChild(dummy, createTextNode(myDoc, " lalal"))
!!$  dummy => insertBefore(np2, dummy, null())
!!$  dummy => appendChild(np2, cloneNode(np2, .true.))
!!$
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$  dummy => appendChild(np2, createTextNode(myDoc, 'a'))
!!$
!!$  call normalize(np2)
!!$
!!$  call serialize(myDoc, "out321.xml")
!!$  call dumpTree(myDoc)
!!$!  call dumpTree(myDoc)
!!$
!!$  interest = getElementsByTagName(myDoc, 'a')

  call destroyDocument(myDoc)
!  call destroyDocument(myOtherDoc)
end program dom_example
