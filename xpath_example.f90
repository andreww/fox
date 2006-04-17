program xpath_example

  ! example driver for querying a document using xpath
  
  use xmlf90_dom, only : fDocumentNode, parsefile, serialize, destroyDocument, dict
  use xmlf90_xpath, only : xpath_query

  implicit none

  type(dict) :: nsDict
  type(fDocumentNode), pointer :: document
  type(xpath_type) :: result

  document => parsefile("test.xml")

  call add_item(nsDict, "cml", "http://www.xml-cml.org/schema")

  result = xpath_query(document, &
    "/cml:cml/cml:parameterList[position()=1]/parameter[@dictRef='testParam']/scalar/text()", &
    nsDict)
  print*, xpath_as_string(result) ! since it is text we are interested in

  result = xpath_query(document, &
    "count(/cml:cml/cml:module[@title='Initialization'])", &
    nsDict)
  print*, xpath_as_number(result) ! since it is a number we are interested in

  result = xpath_query(document, 
    "/cml:cml/cml:module[@title='Initialization']", &
    nsDict)
  print*, xpath_as_nodeset(result) ! since it is a nodeset

  ! and we can reuse the nodeset to search again ...
  result = xpath_query(result, &
    "cml:molecule/cml:atomArray/cml:atom[@elementName='O']", &
    nsDict)
  print*, xpath_as_boolean(result) ! does the previous module contain an O atom?

  call destroyDocument(document)

end program example














