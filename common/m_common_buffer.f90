module m_common_buffer

  use m_common_charset, only: XML1_0, whitespace
  use m_common_error, only: FoX_error, FoX_warning
  use m_common_format, only: str

  implicit none
  private
  
  ! At this point we use a fixed-size buffer. 
  ! Note however that buffer overflows will only be
  ! triggered by overly long *unbroken* pcdata values, or
  ! by overly long attribute values. Hopefully
  ! element or attribute names are "short enough".
  !
  ! In a forthcoming implementation it could be made dynamical...
  
  ! MAX_BUFF_SIZE cannot be bigger than the maximum available
  ! record length for a compiler. In practice, this means
  ! 1024 seems to be the biggest available size.
  
  integer, parameter :: MAX_BUFF_SIZE  = 1024
  integer, parameter :: BUFF_SIZE_WARNING  = 0.9 * MAX_BUFF_SIZE
  
  type buffer_t
    private
    integer                       :: size
    character(len=MAX_BUFF_SIZE)  :: str
    integer                       :: unit
    integer                       :: xml_version
  end type buffer_t
  
  public :: buffer_t
  
  public :: add_to_buffer
  public :: print_buffer, str, char, len
  public :: buffer_to_chararray
  public :: buffer_nearly_full
  public :: reset_buffer
  public :: dump_buffer

  interface str
    module procedure buffer_to_str
  end interface
  
  interface char
    module procedure buffer_to_str
  end interface
  
  interface len
    module procedure buffer_length
  end interface
  
contains

  subroutine add_to_buffer(s, buffer, ws_significant)
!FIXME we should call check_buffer from here.
    character(len=*), intent(in)   :: s
    type(buffer_t), intent(inout)  :: buffer
    logical, intent(in), optional :: ws_significant
    
    integer   :: i, n, len_b
    character(len=(buffer%size+len(s))) :: s2

    ! Is whitespace significant in this context?
    ! If not, we can adjust the buffer without worrying
    ! about it.
    ! But if we are not told, we warn about it.
    ! And if we are told it definitely is - then we error out.

    !If we overreach our buffer size, we will be unable to
    !output any more characters without a newline.
    ! Go through new string, insert newlines
    ! at spaces just before MAX_BUFF_SIZE chars
    ! until we have less than MAX_BUFF_SIZE left to go,
    ! then put that in the buffer.

    ! If no whitespace is found in the newly-added string, then
    ! insert a new line immediately before it (at the end of the
    ! current buffer)

    call check_buffer(s, buffer%xml_version)

    if (buffer%size + len(s) > MAX_BUFF_SIZE) then
      if (.not.present(ws_significant)) then
        call FoX_warning( &
        "Output buffer too small. Need to insert a newline. If whitespace might be significant, check your output.")
      elseif (ws_significant) then
        call FoX_error( &
        "Output buffer too small. Need to insert a newline but whitespace is significant. Stopping now.")
      else
        continue ! without error or warning
      endif
    endif

    ! FIXME this is wrong. We should not ever insert whitespace into the
    ! existing buffer, we don't know if it might be whitespace-significant.

    s2 = buffer%str(:buffer%size)//s

    n = 1
    do while (n < len(s2)-MAX_BUFF_SIZE)
      i = scan(s2(n:n+MAX_BUFF_SIZE-1), whitespace, back=.true.)
      write(buffer%unit, '(a)') s2(n:n+i-1)
      n = n + i 
    enddo

    len_b = len(s2) - n + 1
    buffer%str(:len_b) = s2(n:)
    buffer%size = len_b

  end subroutine add_to_buffer


  subroutine reset_buffer(buffer, unit, xml_version)
    type(buffer_t), intent(inout)  :: buffer
    integer, intent(in), optional :: unit
    integer, intent(in) :: xml_version

    buffer%size = 0
    if (present(unit)) then
      buffer%unit = unit
    else 
      buffer%unit = 6
    endif
    buffer%xml_version = xml_version
    
  end subroutine reset_buffer
  

  subroutine print_buffer(buffer)
    type(buffer_t), intent(in)  :: buffer
    
    write(unit=6,fmt="(a)") buffer%str(:buffer%size)

  end subroutine print_buffer


  function buffer_to_str(buffer) result(str)
    type(buffer_t), intent(in)          :: buffer
    character(len=buffer%size)          :: str
    
    str = buffer%str(:buffer%size)
  end function buffer_to_str


  function buffer_to_chararray(buffer) result(str)
    type(buffer_t), intent(in)               :: buffer
    character(len=1), dimension(buffer%size) :: str
    integer :: i
    
    do i = 1, buffer%size
      str(i) = buffer%str(i:i)
    enddo
  end function buffer_to_chararray


  function buffer_nearly_full(buffer) result(warn)
    type(buffer_t), intent(in)          :: buffer
    logical                             :: warn
    
    warn = buffer%size > BUFF_SIZE_WARNING
    
  end function buffer_nearly_full


  function buffer_length(buffer) result(length)
    type(buffer_t), intent(in)          :: buffer
    integer                             :: length
    
    length = buffer%size 

  end function buffer_length

  
  subroutine dump_buffer(buffer, lf)
    type(buffer_t), intent(inout) :: buffer
    logical, intent(in), optional :: lf

    logical :: lf_

    if (present(lf)) then
      lf_ = lf
    else
      lf_ = .true.
    endif

    if (lf_) then
      write(buffer%unit, '(a)') buffer%str(:buffer%size)
    else
      write(buffer%unit, '(a)', advance='no') buffer%str(:buffer%size)
    endif
    buffer%size = 0
  end subroutine dump_buffer

  
  subroutine check_buffer(s, version)
    character(len=*), intent(in) :: s
    integer, intent(in) :: version

    integer :: i

!FIXME this is almost a duplicate of logic in wxml/m_wxml_escape.f90

    ! We have to do it this way (with achar etc) in case the native
    ! platform encoding is not ASCII

    do i = 1, len(s)
      select case (iachar(s(i:i)))
      case (0)
        call FoX_error("Tried to output a NUL character")
      case (1:8,11:12,14:31)
        if (version==XML1_0) then
          call FoX_error("Tried to output a character invalid under XML 1.0: &#"//str(iachar(s(i:i)))//";")
        endif
      case (128:)
        !TOHW we should maybe just disallow this ...
        call FoX_warning("emitting non-ASCII character. Platform-dependent result!")
      end select
    enddo 

  end subroutine check_buffer


end module m_common_buffer
