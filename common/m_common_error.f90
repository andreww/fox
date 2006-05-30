module m_common_error

  use pxf
  implicit none

  integer, parameter :: WARNING_CODE       = 0
  integer, parameter :: SEVERE_ERROR_CODE  =1 

  public :: general_error

  public :: WARNING_CODE
  public :: SEVERE_ERROR_CODE

contains

  subroutine general_error(msg,code)
    character(len=*), intent(in)     :: msg
    integer, intent(in)              :: code

     if (code == SEVERE_ERROR_CODE)  then
       write(unit=0,fmt="(2a)") "** Error: ", msg
       call pxfflush(0)
       call pxfflush(6)
       call pxfabort()
     else if (code == WARNING_CODE)  then
       write(unit=0,fmt="(2a)") "** Warning: ", msg
       call pxfflush(0)
     else
       write(0,'(a)') "wrong error code"
       call pxfflush(0)
       call pxfabort()
     endif
   end subroutine general_error

end module m_common_error
