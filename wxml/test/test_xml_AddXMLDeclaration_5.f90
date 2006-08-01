program test

  use FoX_wxml, only : xmlf_t, xml_OpenFile, xml_Close
  use FoX_wxml, only : xml_AddXMLDeclaration
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call xml_OpenFile(filename, xf, addDecl=.false.)
  call xml_AddXMLDeclaration(xf, encoding='***')
  call xml_Close(xf)

end program test
