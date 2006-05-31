module m_sax_error
!
! Error handling
!
use m_common_elstack
private

type, public  :: sax_error_t
      character(len=100)  :: message
      integer             :: line
      integer             :: column
      type(elstack_t)     :: stack
      integer             :: severity
end type sax_error_t

integer, public             ::  sax_stderr = 0    ! Unit for error info
integer, public, parameter  ::  SEVERE_ERROR_CODE=0, WARNING_CODE=1

public  :: build_error_info, default_error_handler
public  :: set_sax_stderr

CONTAINS

!-------------------------------------------------------------------------
subroutine build_error_info(error_info,message,line,column,stack,severity)
type(sax_error_t), intent(out)        :: error_info
integer, intent(in)                   :: line, column
character(len=*), intent(in)          :: message
type(elstack_t), intent(in)           :: stack
integer, intent(in)                   :: severity

error_info%message = message
error_info%line = line
error_info%column = column
error_info%stack = stack
error_info%severity = severity

end subroutine build_error_info

!--------------------------------------------------

subroutine default_error_handler(error_info)
type(sax_error_t), intent(in)            :: error_info
!
! Default error handling
!
if (error_info%severity == SEVERE_ERROR_CODE)  then
   write(unit=sax_stderr,fmt="(a)") "*** XML parsing Error:"
else if (error_info%severity == WARNING_CODE)  then
   write(unit=sax_stderr,fmt="(a)") "*** XML parsing Warning:"
endif
write(unit=sax_stderr,fmt="(a)") trim(error_info%message)
write(unit=sax_stderr,fmt="(a,i8,a,i4)") "Line: ", &
                                         error_info%line, &
                                         " Column: ", &
                                         error_info%column
write(unit=sax_stderr,fmt="(a)") "Element traceback:"
call print_elstack(error_info%stack,unit=sax_stderr)
!
!   If there is a severe error the program should stop...
!
if (error_info%severity == SEVERE_ERROR_CODE)  then
      STOP
else if (error_info%severity == WARNING_CODE)  then
   write(unit=sax_stderr,fmt="(a)") "*** Continuing after Warning..."
endif

end subroutine default_error_handler

!-------------------------------------------------------------------------
subroutine set_sax_stderr(unit)
integer, intent(in)  :: unit

sax_stderr  = unit

end subroutine set_sax_stderr

end module m_sax_error
