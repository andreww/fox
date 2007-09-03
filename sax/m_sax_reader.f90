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


  use m_common_array_str, only : vs_str, str_vs, vs_str_alloc
  use m_common_charset, only: XML1_0, XML1_1
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




  interface index
    module procedure index_fb
  end interface
  interface scan
    module procedure scan_fb
  end interface
  interface verify
    module procedure verify_fb
  end interface


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
  public :: get_characters_until_condition
  public :: get_characters_until_not_one_of
  public :: get_characters_until_one_of
  public :: get_characters_until_all_of
  public :: put_characters

  public :: next_chars_are

  public :: dump_string

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
        call put_characters(fb, 1)
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
        rc = fb%next_chars(1)
        allocate(nc(size(fb%next_chars)-1))
        nc = fb%next_chars(2:)
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

    allocate(nc(size(fb%next_chars)+len(s)))
    nc(:size(fb%next_chars)) = fb%next_chars
    nc(size(fb%next_chars)+1:) = vs_str(s)
    deallocate(fb%next_chars)
    fb%next_chars => nc
  end subroutine push_chars

  ! This subroutine will read in 512 chars; do XML-compliant new-line
  ! replacement (shortening the string) and repeat until either it has
  ! filled a 512-length string, it encounters a file error, or eof.
  ! In every loop of the recursion, we may have to read one extra
  ! char to check it. If we do & need to keep it around, we put it back.

  ! It will return a 512-length string. Output from the subroutine
  ! are l_s, the number of chars in the string (always 512 except in
  ! case of eof), iostat (either zero, eof, or error).

  ! It will assume XML-1.0 end of line semantics, unless version 1.1
  ! is specified.

  subroutine fill_buffer(fb, iostat)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat

    integer :: l_s ! number of chars read
    integer :: z ! number of chars left to be read

    ! This subroutine has three layers.

    ! read_really actually interacts with the file and provides
    ! raw characters. It reads lines repeatedly, with non-advancing
    ! IO, adding 0xD every time it hits a newline (== end of record)
    ! until enough characters have been gathered.

    ! fill_buffer_really fills the buffer with READLENGTH chars from 
    ! read_really, then does newline replacement, shortening the
    ! string in the buffer accordingly.

    ! The main part of the subroutine then calls fill_buffer_really
    ! repeatedly until we've definitely got 512 characters after
    ! newline replacement.

    ! The buffer should then only be accessed through one of the 
    ! get_characters routine below, which take care of the pushback
    ! buffer.

    ! fill_buffer returns an iostat. This is zero if ok - if we have
    ! encountered an eof but there are still characters in the buffer,
    ! it is still zero. It is only non-zero when the buffer is empty,
    ! and we've hit eof - or when we've hit an IO error in reading.

    if (fb%pos > READLENGTH) then
      fb%buffer(:READLENGTH) = fb%buffer(READLENGTH+1:)
      fb%pos = fb%pos - READLENGTH
      fb%nchars = fb%nchars - READLENGTH
    endif

    l_s = 0
    z = READLENGTH - l_s

    do while (z>0)
      call fill_buffer_really
      if (iostat/=0) exit
    enddo

    fb%nchars = fb%nchars + l_s

    if (iostat == io_eof) then
      if (fb%nchars - fb%pos >= 0) iostat = 0
    endif

  contains

    subroutine fill_buffer_really

      integer :: i, n, p, q
      character :: c
      character(len=z) :: string

      if (z==0) then
        iostat = 0
        return
      endif

      string = read_really(z, p)
      ! p now holds useful length of current string
      if (iostat /= 0) return

      ! Now replace newline characters according to 
      ! XML 1.0/1.1 section 2.11
      i = 1
      if (fb%xml_version == XML1_0) then
        n = scan(string(:p), achar(10))
      elseif (fb%xml_version == XML1_0) then
        n = scan(string(:p), achar(10)//achar(133))
      endif
      do while (n > 0) 
        if (n == p) then
          ! We're at the end of the string ... we need the next character
          c = read_really(1, q)
          ! unless this is the end of the file
          if (iostat /= 0) then
            if ((fb%xml_version==XML1_0.and.c/=achar(13)).or. &
              (fb%xml_version==XML1_1.and.c/=achar(13).and.c/=achar(133))) then
              ! FIXME we need to store the character somewhere.
              continue
              ! else we'd be throwing it away below anyway
            endif
          else
            if (iostat /= io_eof) return
            ! Hard error, return
          endif
          ! else it really is the end of the file, carry on for the moment.
          ! Now: it's not the end of the string, what's the next character?
        elseif ((fb%xml_version==XML1_0.and.string(n+1:n+1)==achar(13)).or. &
          (fb%xml_version==XML1_1.and.string(n+1:n+1)/=achar(13).and.string(n+1:n+1)/=achar(133))) then
          ! We must discard it, and thus shorten the string we have.
          string(n+1:p-1) = string(n+2:p)
          p = p - 1
        endif
        ! Finally, we must change the 
        ! change the newline char to achar(13) & finish.
        string(n:n) = achar(13)
        ! and find the next one ...
        if (fb%xml_version == XML1_0) then
          n = index(string(n+1:p), achar(10))
        elseif (fb%xml_version == XML1_0) then
          n = scan(string(n+1:p), achar(10)//achar(133))
        endif
      enddo

      fb%buffer(fb%nchars+l_s+1:fb%nchars+l_s+p) = string(:p)
      l_s = l_s + p
      z = READLENGTH - l_s

    end subroutine fill_buffer_really

    function read_really(n_chars, ncr) result (string)
      integer, intent(in) :: n_chars
      integer, intent(out) :: ncr ! number characters read
      character(len=n_chars) :: string
      integer :: nc

      ! This should only return EOF if it is EOF , and we've read no
      ! character at all.


      ! We may have a pending newline
      if (fb%eor) then
        string(1:1) = achar(13)
        fb%eor = .false.
        ncr = 1
      else
        ncr = 0
      endif
      do while (n_chars > ncr)
        if (fb%eof) then
          ! Don't read again if we've already encountered eof
          nc = 0
          if (ncr==0) then
            iostat = io_eof
            return
          endif
          ! else we don't want to return just yet - we might have
          ! picked up some chars from previous reads
        else
          if (fb%lun == -1) then
            ! get chars from string
            if (size(fb%input_string) - fb%input_pos + 1 < n_chars) then
              iostat = io_eof
              nc = size(fb%input_string) - fb%input_pos + 1
              string(:nc) = str_vs(fb%input_string(fb%input_pos:))
              fb%input_pos = size(fb%input_string) + 1
            else
              iostat = 0
              nc = n_chars
              string(:nc) = str_vs(fb%input_string(fb%input_pos:fb%input_pos+n_chars-1))
              fb%input_pos = fb%input_pos+n_chars
            endif
          else
            read(unit=fb%lun, iostat=iostat, advance="no", &
              size=nc, fmt="("//str(n_chars-ncr)//"a1)") string(ncr+1:)
          endif
        endif
        ncr = ncr + nc
        if (iostat==io_eor) then ! we hit a newline, need to record it
          if (ncr < n_chars) then
            ! we can just add it to the string
            ncr = ncr + 1
            string(ncr:ncr) = achar(13)
          else
            ! we already have enough characters, remember for next time.
            fb%eor = .true.
          endif
          iostat = 0 ! and forget about the error
        elseif (iostat == io_eof.and.ncr > 0) then
          fb%eof = .true.
          iostat = 0 ! since we're still returning some characters
          return
        elseif (iostat/=0) then
          !either we've read an eof, or we've hit an error. Either way:
          return
        endif
      enddo

    end function read_really

  end subroutine fill_buffer

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

    integer :: n_held
    type(buffer_t), pointer :: cb

    if (.not.fb%connected) then
      iostat = BUFFER_NOT_CONNECTED
      return
    endif

    ! Which is current buffer?
    if (size(fb%buffer_stack) > 0) then
      cb => fb%buffer_stack(1)

      n_held = size(cb%s) - cb%pos + 1
      if (n <= n_held) then
        string = str_vs(cb%s(cb%pos:cb%pos+n-1))
        cb%pos = cb%pos + n
        iostat = 0 
      else
        ! Not enough characters here
        string = ''
        iostat = io_eof
        return
      endif
    else
      ! We're reading straight from the file.
      ! Refill the buffer if necessary
      if ((fb%nchars-fb%pos)<READLENGTH) then
        ! This will happen if a) this is the 1st/2nd time we are called
        ! b) Last buffer access was through a get_characters_until...
        call fill_buffer(fb, iostat)
        if (iostat/=0) return
      endif
      if (n <= fb%nchars-fb%pos+1) then
        string = fb%buffer(fb%pos:fb%pos+n)
        fb%pos = fb%pos + n
        iostat = 0
      else
        ! Not enough characters left
        string = ''
        iostat = io_eof
        return
      endif
    endif

    call move_cursor(fb, string)

  end function get_characters


  subroutine get_characters_until_condition(fb, condition, true, iostat)
    type(file_buffer_t), intent(inout) :: fb
    interface
      function condition(c) result(p)
        character, intent(in) :: c
        logical :: p
      end function condition
    end interface
    logical, intent(in) :: true
    integer, intent(out) :: iostat

    type(buffer_t), pointer :: cb
    character, dimension(:), pointer :: tempbuf, buf
    integer :: m_i


    if (size(fb%buffer_stack)>0) then
      cb => fb%buffer_stack(1)
      if (cb%pos>size(cb%s)) then
        iostat = io_eof
        return
      endif
    elseif (fb%eof.and.fb%pos>fb%nchars) then
      iostat = io_eof
      return
    endif

    allocate(buf(0))
    m_i = check_fb(fb, condition, true)
    do while (size(fb%buffer_stack)==0.and.m_i==0)
      allocate(tempbuf(size(buf)+fb%nchars-fb%pos+1))
      tempbuf(:size(buf)) = buf
      tempbuf(size(buf)+1:) = vs_str(fb%buffer(fb%pos:fb%nchars))
      deallocate(buf)
      buf => tempbuf
      fb%pos = fb%nchars + 1
      call fill_buffer(fb, iostat)
      if (iostat==io_eof) then
        iostat = 0! just return with what we have
        m_i = fb%nchars - fb%pos + 1
        if (m_i==0) then 
          m_i = 1
        else
          iostat = 0 
        endif
        exit
      elseif (iostat/=0) then
        return
      else
        m_i = check_fb(fb, condition, true)
      endif
    enddo

    if (size(fb%buffer_stack)>0) then
      deallocate(buf)
      if (m_i==0) then
        allocate(buf(size(cb%s)-cb%pos+1))
        buf = cb%s(cb%pos:)
        cb%pos = size(cb%s) + 1
      else
        allocate(buf(m_i-1))
        buf = cb%s(cb%pos:cb%pos+m_i-1)
        cb%pos = cb%pos + m_i - 1
      endif
    else
      allocate(tempbuf(size(buf)+m_i-1))
      tempbuf(:size(buf)) = buf
      tempbuf(size(buf)+1:) = vs_str(fb%buffer(fb%pos:fb%pos+m_i-2))
      deallocate(buf)
      buf => tempbuf
      fb%pos = fb%pos + m_i - 1
    endif

    if (associated(fb%namebuffer)) deallocate(fb%namebuffer)
    fb%namebuffer => buf
    call move_cursor(fb, str_vs(fb%namebuffer))

  end subroutine get_characters_until_condition


  subroutine get_chars_with_condition(fb, marker, condition, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: marker
    integer, external :: condition
    integer, intent(out) :: iostat

    type(buffer_t), pointer :: cb
    character, dimension(:), pointer :: tempbuf, buf
    integer :: m_i

    if (size(fb%buffer_stack)>0) then
      cb => fb%buffer_stack(1)
      if (cb%pos>size(cb%s)) then
        iostat = io_eof
        return
      endif
    elseif (fb%eof.and.fb%pos>fb%nchars) then
      iostat = io_eof
      return
    endif

    allocate(buf(0))
    m_i = condition(fb, marker)
    do while (size(fb%buffer_stack)==0.and.m_i==0)
      allocate(tempbuf(size(buf)+fb%nchars-fb%pos+1))
      tempbuf(:size(buf)) = buf
      tempbuf(size(buf)+1:) = vs_str(fb%buffer(fb%pos:fb%nchars))
      deallocate(buf)
      buf => tempbuf
      fb%pos = fb%nchars + 1
      call fill_buffer(fb, iostat)
      if (iostat==io_eof) then
        m_i = fb%nchars - fb%pos + 1
        if (m_i==0) then 
          m_i = 1
        else
          iostat = 0 
        endif
      elseif (iostat/=0) then
        return
      else
        m_i = condition(fb, marker)
      endif
    enddo

    if (size(fb%buffer_stack)>0) then
      deallocate(buf)
      if (m_i==0) then
        allocate(buf(size(cb%s)-cb%pos+1))
        buf = cb%s(cb%pos:)
        cb%pos = size(cb%s)+1
      else
        allocate(buf(m_i-1))
        buf = cb%s(cb%pos:cb%pos+m_i-1)
        cb%pos = cb%pos + m_i - 1
      endif
    else
      allocate(tempbuf(size(buf)+m_i-1))
      tempbuf(:size(buf)) = buf
      tempbuf(size(buf)+1:) = vs_str(fb%buffer(fb%pos:fb%pos+m_i-2))
      deallocate(buf)
      buf => tempbuf
      fb%pos = fb%pos + m_i - 1
    endif

    if (associated(fb%namebuffer)) deallocate(fb%namebuffer)
    fb%namebuffer => buf
    call move_cursor(fb, str_vs(fb%namebuffer))

  end subroutine get_chars_with_condition


  subroutine get_characters_until_not_one_of(fb, marker, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*) :: marker
    integer, intent(out) :: iostat

    call get_chars_with_condition(fb, marker, verify_fb, iostat)

  end subroutine get_characters_until_not_one_of

  subroutine get_characters_until_one_of(fb, marker, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: marker
    integer, intent(out) :: iostat

    call get_chars_with_condition(fb, marker, scan_fb, iostat)

  end subroutine get_characters_until_one_of

  subroutine get_characters_until_all_of(fb, marker, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: marker
    integer, intent(out) :: iostat

    call get_chars_with_condition(fb, marker, index_fb, iostat)

  end subroutine get_characters_until_all_of

  function next_chars_are(fb, string, iostat) result(p)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: string
    integer, intent(out) :: iostat
    logical :: p

    character(len=len(string)) :: s

    p = .false.
    s = get_characters(fb, len(string), iostat)
    if (iostat==io_eof) then
      s = ''
      return
    elseif (iostat/=0) then
      return
    endif
    p = (s==string) 
    if (.not.p) call put_characters(fb, len(string))

  end function next_chars_are


  subroutine move_cursor(fb, string)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*) :: string

    integer :: newlines
    newlines = count(vs_str(string) == achar(13))
    if (newlines > 0) then
      fb%line = fb%line + newlines
      fb%col = index(string, achar(13), .true.) - 1
    else
      fb%col = len(string)
    endif

  end subroutine move_cursor


  subroutine put_characters(fb, n)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(in) :: n

    type(buffer_t), pointer :: cb

    if (size(fb%buffer_stack)>0) then
      cb => fb%buffer_stack(1)
      if (n > cb%pos) then
        ! make an error
      else
        cb%pos = cb%pos - n
      endif
    else
      if (n > fb%pos) then
        ! make an error
      else
        fb%pos = fb%pos - n
      endif
    endif

  end subroutine put_characters


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


  subroutine dump_string(string)
    character(len=*) :: string
    integer :: m, n
    m = 1
    n = index(string, achar(13))
    do while (n /= 0)
      m = m + n
      n = index(string(m:), achar(13))
    enddo
  end subroutine dump_string

  function index_fb(fb, marker) result(p)
    type(file_buffer_t), intent(in) :: fb
    character(len=*), intent(in) :: marker
    integer :: p

    type(buffer_t), pointer :: cb

    if (size(fb%buffer_stack) > 0) then
      cb => fb%buffer_stack(1)
      p = index(str_vs(cb%s(cb%pos:)), marker)
    else
      p = index(fb%buffer(fb%pos:fb%nchars), marker)
    endif

  end function index_fb

  function scan_fb(fb, marker) result(p)
    type(file_buffer_t), intent(in) :: fb
    character(len=*), intent(in) :: marker
    integer :: p

    type(buffer_t), pointer :: cb

    if (size(fb%buffer_stack) > 0) then
      cb => fb%buffer_stack(1)
      p = scan(str_vs(cb%s(cb%pos:)), marker)
    else
      p = scan(fb%buffer(fb%pos:fb%nchars), marker)
    endif

  end function scan_fb

  function verify_fb(fb, marker) result(p)
    type(file_buffer_t), intent(in) :: fb
    character(len=*), intent(in) :: marker
    integer :: p

    type(buffer_t), pointer :: cb

    if (size(fb%buffer_stack) > 0) then
      cb => fb%buffer_stack(1)
      p = verify(str_vs(cb%s(cb%pos:)), marker)
    else
      p = verify(fb%buffer(fb%pos:fb%nchars), marker)
    endif

  end function verify_fb

  function check_fb(fb, check, true) result(n)
    type(file_buffer_t), intent(in) :: fb
    interface
      function check(c) result(p)
        character, intent(in) :: c
        logical :: p
      end function check
    end interface
    logical, intent(in) :: true
    integer :: n

    type(buffer_t), pointer :: cb
    integer :: i

    n = 0
    i = 1
    if (size(fb%buffer_stack) > 0) then
      cb => fb%buffer_stack(1)
      do while (cb%pos+i-1<=size(cb%s))
        if (true.eqv.check(cb%s(cb%pos+i-1))) then
          n = i
          exit
        else
          i = i + 1
        endif
      enddo
    else
      do while (fb%pos+i-1<=fb%nchars)
        if (true.eqv.check(fb%buffer(fb%pos+i-1:fb%pos+i-1))) then
          n = i
          exit
        else
          i = i + 1
        endif
      enddo
    endif

  end function check_fb

end module m_sax_reader
