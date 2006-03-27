module m_xpath_error

  ! Xpath error handling routines

  use pxf, only : pxfabort

  implicit none
  private

  public :: xpath_error
  public :: xpath_assert

contains

  subroutine xpath_error(string)
    character(len=*), intent(in) :: string
    
    write(0,'(a)') string
    call pxfabort()
  end subroutine xpath_error

  subroutine xpath_assert(test, string)
    logical, intent(in) :: test
    character(len=*), intent(in) :: string

    if (.not.test) then
      write(0,'(a)') string
      call pxfabort()
    endif

  end subroutine xpath_assert

end module m_xpath_error
