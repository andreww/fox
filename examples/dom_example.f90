program dom_example

  use FoX_dom

  type(fDocumentNode), pointer :: myDoc

  myDoc => parsefile('test.xml')

end program dom_example
