program test

  use FoX_wcml, only : xmlf_t, cmlFinishFile
  implicit none

  character(len=*), parameter :: filename = 'test.xml'
  type(xmlf_t) :: xf

  call cmlFinishFile(xf)

end program test
