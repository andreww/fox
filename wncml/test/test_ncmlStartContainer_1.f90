program test

  use m_wncml_xml 
  use FoX_wxml

  implicit none

  type(xmlf_t) :: xf 

  call xml_OpenFile(filename='test.xml', xf=xf, unit=-1)
  call ncmlStartContainer( xf, 'http://www.example.com/testfile.xml')
  call xml_Close(xf)

end program test
