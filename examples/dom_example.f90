program dom_example

  use m_common_array_str
  use FoX_dom
  implicit none

  type(Node), pointer :: myDoc, np, np2, dummy
  type(NodeList) :: interest

  myDoc => parsefile('test.xml')

  call serialize(myDoc, "out321.xml")

  call destroyDocument(myDoc)

end program dom_example
