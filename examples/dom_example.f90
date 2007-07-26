program dom_example

  use m_common_array_str
  use m_dom_utils, only: dumpTree
  use FoX_dom
  implicit none

  type(DOMImplementation), pointer :: imp
  type(Node), pointer :: myDoc, myOtherDoc, np, np2, dummy
  type(NodeList) :: interest
  integer :: i

  myDoc => parseFile("testNS.xml", "entities")

  call dumpTree(myDoc)
  call serialize(myDoc, "out.xml")

  call destroyDocument(myDoc)
end program dom_example
