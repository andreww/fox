module m_wxml_error

use pxf, only: pxfabort

implicit none

interface wxml_warning
  module procedure wxml_warning_base
end interface

interface wxml_error
  module procedure wxml_error_base
end interface

interface wxml_fatal
  module procedure wxml_fatal_base
end interface

contains
!---------------------------------------------------------
! Error handling/trapping routines:

    subroutine wxml_warning_base(msg)
      ! Emit warning, but carry on.
      character(len=*), intent(in) :: msg

      write(6,'(a)') 'WARNING(wxml)'
      write(6,'(a)')  msg

    end subroutine wxml_warning_base

    subroutine wxml_error_base(msg)
      ! Emit error message, clean up file and stop.
      character(len=*), intent(in) :: msg

      write(6,'(a)') 'ERROR(wxml)'
      write(6,'(a)')  msg

      stop

    end subroutine wxml_error_base

    subroutine wxml_fatal_base(msg)
      !Emit error message and abort with coredump. Does not try to
      !close file, so should be used from anything xml_Close might
      !itself call (to avoid infinite recursion!)

      character(len=*), intent(in) :: msg

      write(6,'(a)') 'ERROR(wxml)'
      write(6,'(a)')  msg

      call pxfabort
      stop

    end subroutine wxml_fatal_base

end module m_wxml_error
