program test_xml_Openfile

  use FoX_wcml
  implicit none

  character(len=*), parameter :: filename = 'test.xml'

  type(xmlf_t) :: xf1

  call cmlBeginFile(xf1, filename, unit=20)

  !TOHW FIXME use check 20 is open and attached to filename

end program test_xml_Openfile
