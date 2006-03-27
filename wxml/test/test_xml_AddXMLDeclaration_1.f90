program test

  use xmlf90_wxml, only : xmlf_t, xml_OpenFile, xml_Close
  use xmlf90_wxml, only : xml_AddXMLDeclaration
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call xml_OpenFile(filename, xf)
  call xml_AddXMLDeclaration(xf)
  call xml_Close(xf)

end program test
