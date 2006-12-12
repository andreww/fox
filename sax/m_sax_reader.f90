module m_sax_reader

  ! This module implements a reader for XML documents.
  ! That is, functionally, it simply takes care of new-line
  ! handling, such that all newlines are converted to ASCII 0xA
  ! according to the algorithm in s2.11 of XML 1.0 or 1.1

  ! It offers several shortcut functions for seeking through
  ! the file for particular sequences of characters useful
  ! for XML parsing.

  ! It also offers the ability to push characters back into 
  ! the reader, which is necessary for readahead by the parser,
  ! and for XML entity references, whose expansion should be
  ! pushed back for reparsing.

  ! In addition, it includes a layer for caching, whereby 
  ! (except under special conditions) it actually reads from 
  ! the file READLENGTH(512) characters at a time, into
  ! a large buffer, which is where new-line conversion takes
  ! place, so most of the seeking is done in memory, not through
  ! IO.


  use m_common_charset, only: XML1_0, XML1_1, &
    XML1_0_NAMECHARS, XML1_1_NAMECHARS, XML_WHITESPACE, &
    XML1_0_INITIALNAMECHARS, XML1_1_INITIALNAMECHARS
  use m_common_error,  only: FoX_error, FoX_fatal
  use m_common_io, only: setup_io, io_eor, io_eof, get_unit
  use m_common_format, only: str
  use m_common_array_str, only : vs_str, str_vs

  implicit none
  private

  integer, parameter              :: BUFFER_NOT_CONNECTED = -2048
  ! FIXME need to check that io_eor & io_eof cannot be -2048
  ! We need a longish character buffer to a) minimize IO and
  ! b) allow stepping backwards in the lexer/parser.
  integer, parameter             :: BUFLENGTH = 1024
  ! The above must be an even number, and greater than 20
  integer, parameter             :: READLENGTH = BUFLENGTH/2

  type buffer_t
    character, dimension(:), pointer :: s
  end type buffer_t

  type file_buffer_t
    private
    logical                  :: connected=.false.   ! Are we connected?
    logical                  :: xml_decl=.false.    ! Have we seen an XML
    ! declaration yet? - we need to know for whitespace handling.
    integer                  :: xml_version = XML1_0
    integer                  :: lun                 ! Which unit number
    logical                  :: eof                 ! have we read past eof?
    ! Necessary since we don't want to report eof to user until
    ! fb%pos gets there, but we don't want to issue another read.
    character, pointer       :: filename(:)         ! filename
    character(len=BUFLENGTH) :: buffer              ! character buffer.
    type(buffer_t),  pointer :: buffer_stack(:)     ! stack of expansion buffers
    character, pointer       :: namebuffer(:)       ! temporary buffer for
    ! retrieving strings potentially greater than 1024 chars
    character, pointer       :: next_chars(:)       ! Additional arbitrary
    ! length character stack in case necessary (either for one char 
    ! readahead or entity expansion)
    logical                  :: eor                 ! read_really needs to
    ! remember if we have a pending newline
    integer                  :: pos                 ! which char are we at?
    integer                  :: nchars              ! How many chars in buffer?
    integer                  :: line                ! which line of file?
    integer                  :: col                 ! which column of file?
    logical                  :: debug               ! debug messages?
  end type file_buffer_t


  public :: file_buffer_t
  public :: line
  public :: column

  public :: open_file
  public :: rewind_file
  public :: close_file

  public :: read_char
  public :: read_chars

  public :: get_characters
  public :: get_next_character_discarding_whitespace
  public :: get_characters_until_not_one_of
  public :: get_characters_until_one_of
  public :: get_characters_until_all_of
  public :: put_characters

  public :: len_namebuffer
  public :: retrieve_namebuffer

  public :: dump_string

contains

  subroutine open_file(fname, fb, iostat, lun)
    character(len=*), intent(in)      :: fname
    type(file_buffer_t), intent(out)  :: fb
    integer, intent(out)              :: iostat
    integer, intent(in), optional     :: lun
    
    iostat = 0

    call setup_io()
    
    if (present(lun)) then
      fb%lun = iostat
    else
      call get_unit(fb%lun,iostat)
      if (iostat /= 0) then
        if (fb%debug) write(*,'(a)') "Cannot get unit"
        return
      endif
    endif

    if (.true.) then
      fb%debug = .true.
    else
      fb%debug = .false.
    endif

    open(unit=fb%lun, file=fname, form="formatted", status="old", &
        action="read", position="rewind", iostat=iostat)
    if (iostat /= 0) then
      if (fb%debug) write(*,'(a)') "Cannot open file "//fname//" iostat: "//str(iostat)
      return
    endif

    allocate(fb%next_chars(0))

    fb%connected = .true.
    fb%eor = .false.
    fb%eof = .false.
    fb%line = 1
    fb%col = 0
    allocate(fb%filename(len(fname)))
    fb%filename = vs_str(fname)
    fb%pos = 1
    fb%nchars = 0
    allocate(fb%buffer_stack(0))

  end subroutine open_file

  !-------------------------------------------------
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
    deallocate(fb%next_chars)
    allocate(fb%next_chars(0))
    do i = 1, size(fb%buffer_stack)
      deallocate(fb%buffer_stack(i)%s)
    enddo
    deallocate(fb%buffer_stack)

    rewind(unit=fb%lun)

  end subroutine rewind_file


  subroutine close_file(fb)
    type(file_buffer_t), intent(inout)  :: fb

    integer :: i

    if (fb%connected) then
      deallocate(fb%filename)
      deallocate(fb%next_chars)
      if (associated(fb%namebuffer)) deallocate(fb%namebuffer)
      do i = 1, size(fb%buffer_stack)
        deallocate(fb%buffer_stack(i)%s)
      enddo
      close(unit=fb%lun)
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


  function read_char(fb, iostat) result(c)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat
    character :: c

    character :: c2
    character, pointer :: tempString(:)

    c = really_read_char(iostat)
    if (iostat/=0) return
    if (c == achar(10)) then
      c2 = really_read_char(iostat)
      if (iostat/=0) return
      if (c2 /= achar(13)) then
        call put_characters(fb, c2)
        ! else discard it.
      endif
      fb%line = fb%line + 1
      fb%col = 0
      c = achar(13)
    endif
    
  contains
    function really_read_char(iostat) result(rc)
      integer, intent(out) :: iostat
      integer :: n_stack
      character :: rc
      n_stack = size(fb%next_chars)
      if (n_stack > 0) then
        rc = fb%next_chars(1)
        tempString => fb%next_chars
        deallocate(fb%next_chars)
        allocate(fb%next_chars(n_stack-1))
        fb%next_chars = tempString(2:)
        iostat = 0
      else
        read(unit=fb%lun, iostat=iostat, advance="no", fmt="(a1)") rc
        if (iostat == io_eor) then
          rc = achar(13)
          iostat = 0
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

  ! This subroutine will read in 512 chars; do XML-compliant new-line
  ! replacement (shortening the string) and repeat until either it has
  ! filled a 512-length string, it encounters a file error, or eof.
  ! In every loop of the recursion, we may have to read one extra
  ! char to check it. If we do & need to keep it around, it stays
  ! in fb%next_char until the next read.

  ! It will return a 512-length string. Output from the subroutine
  ! are l_s, the number of chars in the string (always 512 except in
  ! case of eof), iostat (either zero, eof, or error).

  ! It will assume XML-1.0 end of line semantics, unless version 1.1
  ! is specified.

  subroutine fill_buffer(fb, iostat)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat

    character(len=READLENGTH) :: string
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

    ! FIXME semantics of pushback buffer ...

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
      if (fb%debug) write(*,'(a)') "End of file."
      if (fb%nchars - fb%pos > 0) iostat = 0
      return 
    elseif (iostat > 0) then
      if (fb%debug) write(*,'(a)') "Hard i/o error. iostat: "//str(iostat)
      return ! FIXME?
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
              call put_characters(fb, c)
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

      ! FIXME actually not sure of this next line...
      fb%buffer(fb%nchars+l_s+1:fb%nchars+l_s+p) = string(:p)
      l_s = l_s + p
      z = READLENGTH - l_s

    end subroutine fill_buffer_really

    function read_really(n_chars, ncr) result (string)
      integer, intent(in) :: n_chars
      integer, intent(out) :: ncr ! number characters read
      character(len=n_chars) :: string
      integer :: nc

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
          iostat = io_eof
          ! We don't want to return just yet - we might have
          ! picked up some chars from previous reads
        else
          read(unit=fb%lun, iostat=iostat, advance="no", &
            size=nc, fmt="("//str(n_chars-ncr)//"a)") string(ncr+1:)
        endif
        ncr = ncr + nc
        if (iostat == io_eor) then ! we hit a newline, need to record it
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

  function get_characters(fb, n, iostat) result(string)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(in) :: n
    integer, intent(out) :: iostat
    character(len=n) :: string

    integer :: n_needed, n_held
    type(buffer_t), pointer :: currentBuffer
    character, pointer :: tempbuf(:)

    if (.not. fb%connected) then
      iostat = BUFFER_NOT_CONNECTED
      return
    endif

    !If we have any characters in the pushback buffer, grab
    !them first.

    ! Which is current buffer?
    if (size(fb%buffer_stack) > 0) then
      currentBuffer => fb%buffer_stack(size(fb%buffer_stack))
      
      n_held = size(currentBuffer%s)
      if (n <= n_held) then
        ! Actually, we should just move a cursor here rather than
        ! reallocating every time.
        string = str_vs(currentBuffer%s(:n))
        allocate(tempbuf(n_held-n))
        tempbuf = currentBuffer%s(n+1:)
        deallocate(currentBuffer%s)
        currentBuffer%s => tempbuf
      else
        ! Not enough characters here
        iostat = io_eof ! will trigger an end of parsing this buffer
      endif
    else
      ! We're reading straight from the file.
      ! Refill the buffer if necessary
      if ((fb%nchars-fb%pos)<=READLENGTH) then
        ! This will happen if a) this is the 1st/2nd time we are called
        ! b) Last buffer access was through a get_characters_until...
        call fill_buffer(fb, iostat)
        if (iostat/=0) return
      endif
      if (fb%nchars-fb%pos+1 > n) then
        string = fb%buffer(fb%pos:fb%pos+n)
        iostat = 0
      else
        ! Not enough characters left
        iostat = io_eof
        string = ''
        return
      endif
    endif

    call move_cursor(fb, string)

  end function get_characters


  function get_next_character_discarding_whitespace(fb, iostat) result(c)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat
    character :: c

    character, dimension(:), pointer :: tempbuf
    integer :: m_i

    ! Run through the buffer checking for first non-whitespace
    ! character - if none found, then refill the buffer.
    ! Repeat until either we find the correct character, or
    ! IO fails.

    m_i = verify(str_vs(fb%next_chars), XML_WHITESPACE)
    if (m_i == 0) then
      !Chuck away the next_chars, and start searching through the buffer
      call move_cursor(fb, str_vs(fb%next_chars))
      deallocate(fb%next_chars)
      allocate(fb%next_chars(0))
      do
        m_i = verify(fb%buffer(fb%pos:fb%nchars), XML_WHITESPACE)
        if (m_i == 0) then
          call move_cursor(fb, fb%buffer(fb%pos:fb%nchars))
          fb%pos = fb%nchars ! FIXME + 1?
          call fill_buffer(fb, iostat)
          if (iostat /= 0) then
            c = achar(0)
            return
          endif
        else
          call move_cursor(fb, fb%buffer(fb%pos:fb%pos+m_i-1))
          exit ! fb%pos+m_i-1 now holds the char we want.
        endif
      enddo
      fb%pos = fb%pos + m_i
      c = fb%buffer(fb%pos-1:fb%pos-1)
    else
      ! In next_chars somewhere, move along & reallocate.
      c = fb%next_chars(m_i)
      call move_cursor(fb, str_vs(fb%next_chars(:m_i)))
      allocate(tempbuf(size(fb%next_chars)-m_i-1))
      tempbuf = fb%next_chars(m_i+1:)
      deallocate(fb%next_chars)
      fb%next_chars => tempbuf
    endif
    
    ! FIXME somewhere we need to be checking every character for allowability.

  end function get_next_character_discarding_whitespace

  subroutine get_characters_until_not_one_of(fb, marker, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*) :: marker
    integer, intent(out) :: iostat
    
    character, dimension(:), pointer :: tempbuf, buf
    integer :: m_i
    ! Run through next_chars and buffer checking for first non-marker
    ! character, filling a buffer as we go - if none found, then 
    ! refill the buffer.
    ! Repeat until either we find such a character, or
    ! IO fails.

    m_i = verify(str_vs(fb%next_chars), marker)
    if (m_i == 0) then
      ! put all of next_chars into holding buffer, and move on.
      buf => fb%next_chars
      allocate(fb%next_chars(0))
      do
        m_i = verify(fb%buffer(fb%pos:fb%nchars), marker)
        if (m_i == 0) then
          tempbuf => buf
          allocate(buf(size(tempbuf)+fb%nchars-fb%pos+1))
          buf(:size(tempbuf)) = tempbuf
          buf(size(tempbuf)+1:) = vs_str(fb%buffer(fb%pos:fb%nchars))
          deallocate(tempbuf)
          fb%pos = fb%nchars + 1
          call fill_buffer(fb, iostat)
          if (iostat /= 0) return
        else
          exit
        endif
      enddo
      tempbuf => buf
      allocate(buf(size(tempbuf)+m_i-1))
      buf(:size(tempbuf)) = tempbuf
      buf(size(tempbuf)+1:) = vs_str(fb%buffer(fb%pos:fb%pos+m_i-1))
      deallocate(tempbuf)
      fb%pos = fb%pos + m_i - 1
    else
      ! scoot along in next_chars
      allocate(buf(m_i-1))
      buf = fb%next_chars(:m_i-1)
      allocate(tempbuf(size(fb%next_chars)-m_i+1))
      tempbuf = fb%next_chars(m_i:)
      deallocate(fb%next_chars)
      fb%next_chars => tempbuf
    endif

    if (associated(fb%namebuffer)) then
      ! it shouldn't be!
      call FoX_error("Internal error in get_characters_until_not_one_of")
    endif
    fb%namebuffer => buf
    call move_cursor(fb, str_vs(fb%namebuffer))
    ! FIXME somewhere we need to be checking every character for allowability.

  end subroutine get_characters_until_not_one_of

  subroutine get_characters_until_one_of(fb, marker, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: marker
    integer, intent(out) :: iostat

    integer :: m_i 
    character, dimension(:), pointer :: tempbuf, buf
    ! Run through the buffer checking for any of the marker
    ! characters, filling another buffer as we go - if none found, then 
    ! refill the main buffer.
    ! Repeat until either we find such a character, or
    ! IO fails.
    m_i = scan(str_vs(fb%next_chars), marker)
    if (m_i == 0) then
      ! put all of next_chars into holding buffer, and move on.
      buf => fb%next_chars
      allocate(fb%next_chars(0))
      do
        m_i = scan(fb%buffer(fb%pos:fb%nchars), marker)
        if (m_i == 0) then
          tempbuf => buf
          allocate(buf(size(tempbuf)+fb%nchars-fb%pos+1))
          buf(:size(tempbuf)) = tempbuf
          buf(size(tempbuf)+1:) = vs_str(fb%buffer(fb%pos:fb%nchars))
          deallocate(tempbuf)
          fb%pos = fb%nchars + 1
          call fill_buffer(fb, iostat)
          if (iostat /= 0) return
        else
          exit
        endif
      enddo
      tempbuf => buf
      allocate(buf(size(tempbuf)+m_i-1))
      buf(:size(tempbuf)) = tempbuf
      buf(size(tempbuf)+1:) = vs_str(fb%buffer(fb%pos:fb%pos+m_i-1))
      deallocate(tempbuf)
      fb%pos = fb%pos + m_i - 1
    else
      ! scoot along in next_chars
      allocate(buf(m_i-1))
      buf = fb%next_chars(:m_i-1)
      allocate(tempbuf(size(fb%next_chars)-m_i+1))
      tempbuf = fb%next_chars(m_i:)
      deallocate(fb%next_chars)
      fb%next_chars => tempbuf
    endif

    if (associated(fb%namebuffer)) then
      ! it shouldn't be!
      call FoX_error("Internal error in get_characters_until_one_of")
    endif
    fb%namebuffer => buf
    call move_cursor(fb, str_vs(fb%namebuffer))
    ! FIXME somewhere we need to be checking every character for allowability.

  end subroutine get_characters_until_one_of

  subroutine get_characters_until_all_of(fb, marker, iostat)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: marker
    integer, intent(out) :: iostat

    integer :: m_i
    character, dimension(:), pointer :: tempBuf, buf
    ! Run through the buffer checking for marker string,
    ! filling a buffer as we go - if not found, then 
    ! refill the buffer (buffer is only half-filled each
    ! time in case marker string spans join.)
    ! Repeat until either we find such a character, or
    ! IO fails.
    m_i = index(str_vs(fb%next_chars), marker)
    if (m_i == 0) then
      ! put all of next_chars into holding buffer, and move on.
      buf => fb%next_chars
      allocate(fb%next_chars(0))
      do
        m_i = scan(fb%buffer(fb%pos:fb%nchars), marker)
        if (m_i == 0) then
          tempbuf => buf
          allocate(buf(size(tempbuf)+fb%nchars-fb%pos+1))
          buf(:size(tempbuf)) = tempbuf
          buf(size(tempbuf)+1:) = vs_str(fb%buffer(fb%pos:fb%nchars))
          deallocate(tempbuf)
          fb%pos = fb%nchars + 1
          call fill_buffer(fb, iostat)
          if (iostat /= 0) return
        else
          exit
        endif
      enddo
      tempbuf => buf
      allocate(buf(size(tempbuf)+m_i-1))
      buf(:size(tempbuf)) = tempbuf
      buf(size(tempbuf)+1:) = vs_str(fb%buffer(fb%pos:fb%pos+m_i-1))
      deallocate(tempbuf)
      fb%pos = fb%pos + m_i - 1
    else
      ! scoot along in next_chars
      allocate(buf(m_i-1))
      buf = fb%next_chars(:m_i-1)
      allocate(tempbuf(size(fb%next_chars)-m_i+1))
      tempbuf = fb%next_chars(m_i:)
      deallocate(fb%next_chars)
      fb%next_chars => tempbuf
    endif

    if (associated(fb%namebuffer)) then
      ! it shouldn't be!
      call FoX_error("Internal error in get_characters_until_all_of")
    endif
    fb%namebuffer => buf
    call move_cursor(fb, str_vs(fb%namebuffer))
    ! FIXME somewhere we need to be checking every character for allowability.

  end subroutine get_characters_until_all_of

  function len_namebuffer(fb) result(n)
    type(file_buffer_t), intent(in) :: fb
    integer :: n

    n = size(fb%namebuffer)
  end function len_namebuffer

  function retrieve_namebuffer(fb) result(string)
    type(file_buffer_t), intent(inout) :: fb
    character(len=size(fb%namebuffer)) :: string
    
    if (.not.associated(fb%namebuffer)) then
      call FoX_error("Don't be an arse, check return values please")
    endif
    string = str_vs(fb%namebuffer)
    deallocate(fb%namebuffer)
  end function retrieve_namebuffer

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


  subroutine put_characters(fb, string)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: string
    
    character, pointer :: tempString(:)
    integer :: n

    tempString => fb%next_chars
    n = size(tempString)
    allocate(fb%next_chars(n+len(string)))
    fb%next_chars(:n) = tempString
    deallocate(tempString)
    fb%next_chars(n+1:) = vs_str(string)

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
      write(*,'(a)') string(m:m+n-1)
      m = m + n
      n = index(string(m:), achar(13))
    enddo
    write(*,'(a)') string(m:)
  end subroutine dump_string

end module m_sax_reader
