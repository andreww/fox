program ncdfcat

  use netcdf
  use wncml
  use FoX_wxml

  implicit none

  type(xmlf_t) :: xf    ! FoX's file hanldle 
  integer      :: ncid  ! NetCDF's file hanedle
  integer      :: ierr  ! For NetCDF errors (ignored)


  print*, 'using NetCDF version:', ncmlLibVers()

  ! Create an xml document
  call xml_OpenFile(filename='test.xml', xf=xf, unit=0, &
                  & pretty_print=.true.)

  ! Open a NetCDF file for reading
  ierr = NF90_OPEN('test.nc', nf90_nowrite, ncid) 

  ! Write NetCDF file header to the xml document 
  call ncmlDumpContainer( xf, ncid, 'test.nc' )

  ! Close files
  ierr = NF90_CLOSE(ncid)  
  call xml_Close(xf)

end program ncdfcat
