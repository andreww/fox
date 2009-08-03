program test

  use FoX_wkml

  implicit none

  type(xmlf_t) :: myfile

  call kmlBeginFile(myfile, "test.xml", -1)
  ! ' ' is not a valid URI, so should probably be escaped using utils/uri stuff.
  call kmlCreatePointStyle(myfile, id='myid', iconhref=' ')
  call kmlFinishFile(myfile)

end program test
