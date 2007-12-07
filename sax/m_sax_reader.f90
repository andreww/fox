module m_sax_reader

  ! This module implements a reader for XML documents.
  ! That is, functionally, it simply takes care of new-line
  ! handling, such that all newlines are converted to ASCII 0xA
  ! according to the algorithm in s2.11 of XML 1.0 or 1.1

  ! It offers several shortcut functions for seeking through
  ! the file for particular sequences of characters useful
  ! for XML parsing.

  ! It also offers the ability to push characters back into 
  ! the reader, onto a stack of additional buffers, which
  ! we need for entity expansion.

  ! Furthermore, readahead and pushback is available on
  ! all buffers.

  ! In addition, it includes a layer for caching, whereby 
  ! (except during entity expansion) it actually reads from 
  ! the file READLENGTH(512) characters at a time, into
  ! a large buffer with all newline conversion already done.
  ! Thus most of the tokenizer/parser's seeking is done in memory, not through
  ! IO.


  use m_common_array_str, only : vs_str, str_vs, vs_str_alloc, vs_vs_alloc
  use m_common_charset, only: XML1_0, XML1_1, isLegalChar, isXML1_0_NameChar, isXML1_1_NameChar
  use m_common_error,  only: FoX_error
  use m_common_io, only: setup_io, io_eor, io_eof, get_unit
  use m_common_format, only: str

  implicit none
  private

  ! FIXME need to check that io_eor & io_eof cannot be -2048
  integer, parameter             :: BUFFER_NOT_CONNECTED = -2048
  ! We need a longish character buffer to a) minimize IO and
  ! b) allow stepping backwards in the lexer/parser.
  integer, parameter             :: BUFLENGTH = 1024
  ! The above must be an even number, and greater than 20
  integer, parameter             :: READLENGTH = BUFLENGTH/2

  type buffer_t
    character, dimension(:), pointer :: s
    integer :: pos = 1
  end type buffer_t

  type file_buffer_t
    !FIXME private
    logical                  :: connected=.false.   ! Are we connected?
    logical                  :: xml_decl=.false.    ! Have we seen an XML
    ! declaration yet? - we need to know for whitespace handling.
    integer                  :: xml_version = XML1_0
    integer                  :: lun                 ! Which unit number
    ! If lun = -1, then we are reading from a string ....
    character, pointer       :: input_string(:) => null()    ! If input is from a string
    integer                  :: input_pos
    logical                  :: eof                 ! have we read past eof?
    ! Necessary since we don't want to report eof to user until
    ! fb%pos gets there, but we don't want to issue another read.
    character, pointer       :: filename(:) => null()        ! filename
    character(len=BUFLENGTH) :: buffer              ! character buffer.
    type(buffer_t),  pointer :: buffer_stack(:) => null()  ! stack of expansion buffers
    character, pointer       :: namebuffer(:) => null()    ! temporary buffer for
    ! retrieving strings potentially greater than 1024 chars
    logical                  :: eor                 ! read_really needs to
    ! remember if we have a pending newline
    character, pointer       :: next_chars(:)  => null()     ! read_really needs its
    ! own pushback buffer
    integer                  :: pos                 ! which char are we at?
    integer                  :: nchars              ! How many chars in buffer?
    integer                  :: line                ! which line of file?
    integer                  :: col                 ! which column of file?
    logical                  :: debug               ! debug messages?
  end type file_buffer_t


  public :: buffer_t
  public :: file_buffer_t
  public :: line
  public :: column

  public :: open_file
  public :: rewind_file
  public :: close_file

  public :: read_char
  public :: read_chars
  public :: push_chars

  public :: get_characters
  public :: get_characters_until_not_namechar
  public :: get_characters_until_not_one_of
  public :: get_characters_until_one_of
  public :: get_characters_until_all_of

  public :: push_buffer_stack
  public :: pop_buffer_stack

contains

  subroutine open_file(fb, iostat, file, lun, string)
    type(file_buffer_t), intent(out)  :: fb
    character(len=*), intent(in), optional :: file
    integer, intent(out)              :: iostat
    integer, intent(in), optional     :: lun
    character(len=*), intent(in), optional :: string

    iostat = 0

    call setup_io()

    if (present(string)) then
      if (present(file)) then
        call FoX_error("Cannot specify both file and string input to open_xml")
      elseif (present(lun)) then
        call FoX_error("Cannot specify lun for string input to open_xml")
      endif
      fb%lun = -1
      fb%input_string => vs_str_alloc(string)
      allocate(fb%filename(0))
      fb%input_pos = 1
    else
      if (present(lun)) then
        fb%lun = lun
      else
        call get_unit(fb%lun, iostat)
        if (iostat/=0) then
          if (fb%debug) write(*,'(a)') "Cannot find free unit"
          return
        endif
      endif
      open(unit=fb%lun, file=file, form="formatted", status="old", &
        action="read", position="rewind", iostat=iostat)
      if (iostat /= 0) then
        if (fb%debug) write(*,'(a)') "Cannot open file "//file//" iostat: "//str(iostat)
        return
      endif
      fb%filename => vs_str_alloc(file)
      allocate(fb%input_string(0))
    endif

    if (.true.) then
      fb%debug = .true.
    else
      fb%debug = .false.
    endif

    fb%connected = .true.
    fb%eor = .false.
    fb%eof = .false.
    fb%line = 1
    fb%col = 0
    fb%pos = 1
    fb%nchars = 0
    allocate(fb%buffer_stack(0))

    allocate(fb%next_chars(0))

  end subroutine open_file


  subroutine rewind_file(fb)
    type(file_buffer_t), intent(inout)  :: fb

    integer :: i

    fb%eor = .false.
    fb%eof = .false.
    fb%line = 1
    fb%col = 0
    fb%pos = 1
    fb%nchars = 0
    fb%buffer = ""
    do i = 1, size(fb%buffer_stack)
      deallocate(fb%buffer_stack(i)%s)
    enddo
    deallocate(fb%buffer_stack)
    allocate(fb%buffer_stack(0))

    deallocate(fb%next_chars)
    allocate(fb%next_chars(0))

    fb%input_pos = 1
    if (fb%lun/=-1) then
      rewind(unit=fb%lun)
    endif

  end subroutine rewind_file


  subroutine close_file(fb)
    type(file_buffer_t), intent(inout)  :: fb

    integer :: i

    if (fb%connected) then
      deallocate(fb%filename)
      if (associated(fb%namebuffer)) deallocate(fb%namebuffer)
      do i = 1, size(fb%buffer_stack)
        deallocate(fb%buffer_stack(i)%s)
      enddo
      deallocate(fb%buffer_stack)
      deallocate(fb%next_chars)
      if (fb%lun/=-1) close(fb%lun)
      deallocate(fb%input_string)
      fb%connected = .false.
    endif

  end subroutine close_file


  ! For the first few characters, we need to be reading directly,
  ! not via the buffering below - in order that we can read the
  ! XML declaration (if it is there) under special rules.
  ! We still need to check the dynamic buffer, because we need
  ! pushback to cope with newline handling

  ! The implementation below is very inefficient, but simple,
  ! since we must be sure not to read past the end of the XML
  ! declaration with it (thereafter, rules change - we may be 
  ! dealing with XML 1.1 newlines.)
  !  
  ! It only needs to be used until we have finished dealing
  ! with the XML declaration though, so inefficiency is not
  ! important

  ! The put_characters mechanism can't be used here, so we 
  ! have an additional way to put characters back in the buffer
  ! through fb%next_chars which are read first.


  function read_char(fb, iostat) result(c)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat
    character :: c

    character :: c2

    c = really_read_char(iostat)
    if (iostat/=0) return
    if (c == achar(10)) then
      c2 = really_read_char(iostat)
      if (iostat/=0) return
      if (c2 /= achar(13)) then
        call push_chars(fb, c2)
        ! else discard it.
      endif
      fb%line = fb%line + 1
      fb%col = 0
      c = achar(13)
    endif

  contains
    function really_read_char(iostat) result(rc)
      integer, intent(out) :: iostat
      character :: rc
      character, dimension(:), pointer :: nc
      if (size(fb%next_chars)>0) then
        iostat = 0
        rc = fb%next_chars(1)
        nc => vs_vs_alloc(fb%next_chars(2:))
        deallocate(fb%next_chars)
        fb%next_chars => nc
      else
        if (fb%lun==-1) then
          if (fb%input_pos>size(fb%input_string)) then
            rc = achar(0)
            iostat = io_eof
          else
            rc = fb%input_string(fb%input_pos)
            fb%input_pos = fb%input_pos + 1
            iostat = 0
          endif
        else
          read(unit=fb%lun, iostat=iostat, advance="no", fmt="(a1)") rc
          if (iostat == io_eor) then
            rc = achar(13)
            iostat = 0
          endif
        endif
        ! Don't need to do any EOF checking here, we are only
        ! reading one char at a time without buffering
      endif
    end function really_read_char
  end function read_char

  function read_chars(fb, n, iostat) result(s)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(in) :: n
    integer, intent(out) :: iostat
    character(len=n) :: s

    integer :: i

    do i = 1, n
      s(i:i) = read_char(fb, iostat)
      if (iostat/=0) return
    enddo
  end function read_chars

  subroutine push_chars(fb, s)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: s
    character, dimension(:), pointer :: nc

    nc => vs_str_alloc(str_vs(fb%next_chars)//s)
    deallocate(fb%next_chars)
    fb%next_chars => nc
  end subroutine push_chars


  subroutine push_buffer_stack(fb, string)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: string

    ! Add a new buffer to the stack.
    integer :: i
    type(buffer_t), dimension(:), pointer :: tempStack

    allocate(tempStack(size(fb%buffer_stack)+1))
    do i = 1, size(fb%buffer_stack)
      tempStack(i+1)%s => fb%buffer_stack(i)%s
      tempStack(i+1)%pos = fb%buffer_stack(i)%pos
    enddo
    tempStack(1)%s => vs_str_alloc(string)
    deallocate(fb%buffer_stack)
    fb%buffer_stack => tempStack
  end subroutine push_buffer_stack

  subroutine pop_buffer_stack(fb)
    type(file_buffer_t), intent(inout) :: fb

    ! Pop active buffer from the stack.
    integer :: i
    type(buffer_t), dimension(:), pointer :: tempStack

    deallocate(fb%buffer_stack(1)%s)
    allocate(tempStack(size(fb%buffer_stack)-1))
    do i = 1, size(tempStack)
      tempStack(i)%s => fb%buffer_stack(i+1)%s
      tempStack(i)%pos = fb%buffer_stack(i+1)%pos
    enddo
    deallocate(fb%buffer_stack)
    fb%buffer_stack => tempStack

  end subroutine pop_buffer_stack

  function get_characters(fb, n, iostat) result(string)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(in) :: n
    integer, intent(out) :: iostat
    character(len=n) :: string

    type(buffer_t), pointer :: cb
    integer :: offset, n_left, n_held
    character, pointer :: temp(:)

    if (.not.fb%connected) then
      iostat = BUFFER_NOT_CONNECTED
      return
    endif
    if (size(fb%next_chars)>0) then
      if (n<size(fb%next_chars)) then
        string = str_vs(fb%next_chars(:n))
        temp => vs_vs_alloc(fb%next_chars(n+1:))
        deallocate(fb%next_chars)
        fb%next_chars => temp
        iostat = 0
        return
      endif
      offset = size(fb%next_chars)
      string(:offset) = str_vs(fb%next_chars)
      deallocate(fb%next_chars)
      fb%next_chars => vs_str_alloc("")
    else
      offset = 0
    endif     
    n_left = n - offset
    if (n_left==0) then
      iostat = 0
      return
    endif
  
    ! Where are we reading from?
    if (size(fb%buffer_stack) > 0) then
      ! We are reading from an internal character buffer.
      cb => fb%buffer_stack(1)
      n_held = size(cb%s) - cb%pos + 1
      if (n_left <= n_held) then
        string(offset+1:) = str_vs(cb%s(cb%pos:cb%pos+n_left-1))
        cb%pos = cb%pos + n_left
        iostat = 0 
      else
        ! Not enough characters here
        string = ''
        ! put characters back on putchar? FIXME
        iostat = io_eof
        return
      endif
    else
      ! We are reading from a file
      string(offset+1:) = get_chars_from_file(fb, n_left, iostat)
      if (iostat/=0) return ! EOF or Error.
    endif

  end function get_characters


  subroutine get_characters_until_not_namechar(fb, xv, iostat)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(in) :: xv
    integer, intent(out) :: iostat

    character, dimension(:), pointer :: tempbuf, buf
    character :: c

    allocate(buf(0))
    do
      c = get_characters(fb, 1, iostat)
      if (iostat/=0) then
        deallocate(buf)
        return
      endif
      if ((xv==XML1_0.and..not.isXML1_0_NameChar(c)) &
        .or.(xv==XML1_1.and..not.isXML1_1_NameChar(c))) then
        call push_chars(fb, c)
        exit
      endif
      allocate(tempbuf(size(buf)+1))
      tempbuf = (/buf, c/)
      deallocate(buf)
      buf => tempbuf
    enddo

    if (associated(fb%namebuffer)) deallocate(fb%namebuffer)
    fb%namebuffer => buf
  end subroutine get_characters_until_not_namechar


  subroutine get_chars_with_condition(fb, marker, condition, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: marker
    integer, intent(out) :: iostat

    interface
      function condition(c1, c2) result(p)
        character, intent(in) :: c1, c2
        logical :: p
      end function condition
    end interface

    character, dimension(:), pointer :: tempbuf, buf
    character :: c

    allocate(buf(0))
    do while (.not.condition(str_vs(buf), marker))
      c = get_characters(fb, 1, iostat)
      if (iostat/=0) then
        deallocate(buf)
        return
      endif
      allocate(tempbuf(size(buf)+1))
      tempbuf = (/buf, c/)
      deallocate(buf)
      buf => tempbuf
    enddo
    
    if (associated(fb%namebuffer)) deallocate(fb%namebuffer)
    fb%namebuffer => buf

  end subroutine get_chars_with_condition

  subroutine get_characters_until_not_one_of(fb, marker, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*) :: marker
    integer, intent(out) :: iostat

    character, pointer :: tempbuf(:)
    character :: c

    call get_chars_with_condition(fb, marker, verify_fb2, iostat)

    if (iostat==0) then
      allocate(tempbuf(size(fb%namebuffer)-1))
      tempbuf = fb%namebuffer(:size(tempbuf))
      c = fb%namebuffer(size(tempbuf)+1)
      deallocate(fb%namebuffer)
      fb%namebuffer => tempbuf
      call push_chars(fb, c)
    endif

  end subroutine get_characters_until_not_one_of

  subroutine get_characters_until_one_of(fb, marker, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: marker
    integer, intent(out) :: iostat

    character, pointer :: tempbuf(:)
    character :: c

    call get_chars_with_condition(fb, marker, scan_fb2, iostat)

    if (iostat==0) then
      allocate(tempbuf(size(fb%namebuffer)-1))
      tempbuf = fb%namebuffer(:size(tempbuf))
      c = fb%namebuffer(size(tempbuf)+1)
      deallocate(fb%namebuffer)
      fb%namebuffer => tempbuf
      call push_chars(fb, c)
    endif

  end subroutine get_characters_until_one_of

  subroutine get_characters_until_all_of(fb, marker, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: marker
    integer, intent(out) :: iostat

    character, pointer :: tempbuf(:)

    call get_chars_with_condition(fb, marker, index_fb2, iostat)

    if (iostat==0) then
      allocate(tempbuf(size(fb%namebuffer)-len(marker)))
      tempbuf = fb%namebuffer(:size(tempbuf))
      deallocate(fb%namebuffer)
      fb%namebuffer => tempbuf
      call push_chars(fb, marker)
    endif

  end subroutine get_characters_until_all_of

  function verify_fb2(c1, c2) result(p)
    character(len=*), intent(in) :: c1, c2
    logical :: p
    p = (verify(c1,c2)/=0)
  end function verify_fb2

  function scan_fb2(c1, c2) result(p)
    character(len=*), intent(in) :: c1, c2
    logical :: p
    p = (scan(c1,c2)/=0)
  end function scan_fb2

  function index_fb2(c1, c2) result(p)
    character(len=*), intent(in) :: c1, c2
    logical :: p
    p = (index(c1,c2)/=0)
  end function index_fb2

  function get_chars_from_file(fb, n, iostat) result(string)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(in) :: n
    integer, intent(out) :: iostat
    character(len=n) :: string

    integer :: i
    character :: c, c2
    logical :: pending

    i = 1
    pending = .false.
    do while (i<=n)
      if (pending) then
        c = c2
        pending = .false.
      else
        read (unit=fb%lun, iostat=iostat, advance="no", fmt="(a1)") c
        if (iostat==io_eor) then
          c = achar(13)
          iostat = 0
        elseif (iostat/=0) then
          ! FIXME do something with the characters we have read.
          return
        endif
      endif
      if (.not.isLegalChar(c, fb%xml_version)) then
        ! FIXME throw an error)
      endif
      if (c==achar(10)) then
        read (unit=fb%lun, iostat=iostat, advance="no", fmt="(a1)") c2
        fb%col = fb%col + 1
        if (iostat==io_eof) then
          ! the file has just ended on a single CR. Report is as a LF.
          ! Ignore the eof just now, it'll be picked up if we need to 
          ! perform another read.
          iostat = 0
          c = achar(13)
          fb%line = fb%line + 1
          fb%col = 0
        elseif (iostat==io_eor.or.c2==achar(13)) then
          ! (We pretend all ends of lines are LF)
          ! We can drop the CR
          c = c2
        elseif (iostat/=0) then
          ! Unknown IO error
          return
        else
          c = achar(13)
          pending = .true.
        endif
      endif
      string(i:i) = c
      i = i + 1
    enddo

    if (pending) then
      ! we have one character left over, put in the pushback buffer
      allocate(fb%next_chars(1))
      fb%next_chars = c2
    endif
  end function get_chars_from_file


  function line(fb) result(n)
    type(file_buffer_t), intent(in) :: fb
    integer :: n

    n = fb%line
  end function line


  function column(fb) result(n)
    type(file_buffer_t), intent(in) :: fb
    integer :: n

    n = fb%col
  end function column


end module m_sax_reader
