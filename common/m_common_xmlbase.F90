module m_common_xmlbase
#ifndef DUMMYLIB

  implicit none
  private

  public :: basedir

contains

  function basedir(s) result(dir)
    ! Given a full path name, extract the directory portion.
    character(len=*), intent(in) :: s
    character(len=index(s, "/", .true.)) :: dir

    integer :: i

    ! FIXME ideally we would un-escape the filename at this point.
    i = index(s, "/", .true.)
    if (i==0) then
      dir = ""
    else
      dir = s(:i)
    endif
  end function basedir

#endif
end module m_common_xmlbase
