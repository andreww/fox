program dom_example

  use FoX_dom, only: Node, parsefile, dumptree

  type(Node), pointer :: myDoc

  myDoc => parsefile('test.xml')

  call dumpTree(myDoc)

end program dom_example
