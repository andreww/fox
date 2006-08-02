module m_common_error

  use pxf, only: pxfabort, pxfflush

  implicit none

  interface FoX_warning
    module procedure FoX_warning_base
  end interface

  interface FoX_error
    module procedure FoX_error_base
  end interface

  interface FoX_fatal
    module procedure FoX_fatal_base
  end interface

contains
  !---------------------------------------------------------
  ! Error handling/trapping routines:

  subroutine FoX_warning_base(msg)
    ! Emit warning, but carry on.
    character(len=*), intent(in) :: msg

    write(0,'(a)') 'WARNING(FoX)'
    write(0,'(a)')  msg
    call pxfflush(0)

  end subroutine FoX_warning_base

  subroutine FoX_error_base(msg)
    ! Emit error message and stop.
    ! No clean up is done here, but this can
    ! be overridden to include clean-up routines
    character(len=*), intent(in) :: msg

    write(0,'(a)') 'ERROR(FoX)'
    write(0,'(a)')  msg
    call pxfflush(0)

    stop

  end subroutine FoX_error_base

  subroutine FoX_fatal_base(msg)
    !Emit error message and abort with coredump.
    !No clean-up occurs

    character(len=*), intent(in) :: msg

    write(0,'(a)') 'ABORT(FOX)'
    write(0,'(a)')  msg
    call pxfflush(0)

    call pxfabort()

  end subroutine FoX_fatal_base

end module m_common_error
