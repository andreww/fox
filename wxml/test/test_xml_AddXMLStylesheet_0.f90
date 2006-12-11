program test

  use FoX_wxml, only : xmlf_t
  use FoX_wxml, only : xml_AddXMLStylesheet
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call xml_AddXMLStylesheet(xf, "display.xsl", "text/stylesheet")

end program test
