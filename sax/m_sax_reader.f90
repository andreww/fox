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

  use m_common_array_str, only: str_vs, vs_str_alloc, vs_vs_alloc
  use m_common_charset, only: XML_WHITESPACE, XML_INITIALENCODINGCHARS, &
    XML_ENCODINGCHARS, XML1_0, XML1_1, isXML1_0_NameChar, isXML1_1_NameChar, &
    isLegalChar, allowed_encoding
  use m_common_error,  only: error_stack, add_error, FoX_error, in_error
  use m_common_format, only: operator(//)
  use m_common_io, only: setup_io, io_eor, io_eof, get_unit

  implicit none
  private

  type buffer_t
    character, dimension(:), pointer :: s
    integer :: pos = 1
  end type buffer_t

  type xml_source_t
    integer            :: lun = -2
    integer            :: xml_version = XML1_0
    character, pointer :: encoding(:) => null()
    character, pointer :: filename(:) => null()
    integer            :: line = 0
    integer            :: col = 0
    character, pointer :: next_chars(:) => null()   ! pushback buffer
    type(buffer_t), pointer :: input_string => null()
  end type xml_source_t

  type file_buffer_t
    !FIXME private
    type(xml_source_t), pointer :: f(:) => null()
    type(buffer_t),  pointer  :: buffer_stack(:) => null()  ! stack of expansion buffers
    character, pointer        :: namebuffer(:) => null()    ! temporary buffer for retrieving strings
    logical                   :: standalone = .false.
  end type file_buffer_t


  public :: file_buffer_t
  public :: line
  public :: column

  public :: open_file
  public :: close_file

  public :: push_chars

  public :: get_characters

  public :: push_buffer_stack
  public :: pop_buffer_stack

contains

  subroutine open_file(fb, iostat, file, lun, string, es)
    type(file_buffer_t), intent(out)  :: fb
    character(len=*), intent(in), optional :: file
    integer, intent(out)              :: iostat
    integer, intent(in), optional     :: lun
    character(len=*), intent(in), optional :: string
    type(error_stack), intent(inout) :: es

    iostat = 0

    call setup_io()
    allocate(fb%f(1))
    if (present(string)) then
      if (present(file)) then
        call FoX_error("Cannot specify both file and string input to open_xml")
      elseif (present(lun)) then
        call FoX_error("Cannot specify lun for string input to open_xml")
      endif
      call open_string_as_file(fb%f(1), string)
    else
      call open_actual_file(fb%f(1), file, iostat, lun)
      if (iostat/=0) return
    endif

    call parse_xml_declaration(fb%f(1), iostat, es, fb%standalone)
    allocate(fb%buffer_stack(0))

  end subroutine open_file

  subroutine open_string_as_file(f, string)
    type(xml_source_t), intent(out)    :: f
    character(len=*), intent(in)     :: string

    f%lun = -1
    f%input_string%s => vs_str_alloc(string)
    f%filename => vs_str_alloc("")

    f%line = 1
    f%col = 0
    allocate(f%next_chars(0))
  end subroutine open_string_as_file

  subroutine open_actual_file(f, file, iostat, lun)
    type(xml_source_t), intent(out)    :: f
    character(len=*), intent(in)     :: file
    integer, intent(out)             :: iostat
    integer, intent(in), optional    :: lun

    if (present(lun)) then
      f%lun = lun
    else
      call get_unit(f%lun, iostat)
      if (iostat/=0) return
    endif
    open(unit=f%lun, file=file, form="formatted", status="old", &
      action="read", position="rewind", iostat=iostat)
    if (iostat/=0) return
    f%filename => vs_str_alloc(file)

    f%line = 1
    f%col = 0
    allocate(f%next_chars(0))
  end subroutine open_actual_file

  subroutine close_file(fb)
    type(file_buffer_t), intent(inout)  :: fb

    integer :: i

    if (fb%f(1)%lun/=-2) then
      if (associated(fb%namebuffer)) deallocate(fb%namebuffer)
      do i = 1, size(fb%buffer_stack)
        deallocate(fb%buffer_stack(i)%s)
      enddo
      deallocate(fb%buffer_stack)
      call close_actual_file(fb%f(1))
      deallocate(fb%f)
    endif

  end subroutine close_file

  subroutine close_actual_file(f)
    type(xml_source_t), intent(inout)    :: f

    if (f%lun/=-1) then
      close(f%lun)
    else
      deallocate(f%input_string%s)
      deallocate(f%input_string)
    endif
    deallocate(f%filename)

    f%line = 0
    f%col = 0
    deallocate(f%next_chars)
  end subroutine close_actual_file


  subroutine push_chars(fb, s)
    type(file_buffer_t), intent(inout) :: fb
    character(len=*), intent(in) :: s

    call push_file_chars(fb%f(1), s)

  end subroutine push_chars

  subroutine push_file_chars(f, s)
    type(xml_source_t), intent(inout) :: f
    character(len=*), intent(in) :: s
    character, dimension(:), pointer :: nc

    nc => vs_str_alloc(str_vs(f%next_chars)//s)
    deallocate(f%next_chars)
    f%next_chars => nc
  end subroutine push_file_chars


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

  function get_characters(fb, n, iostat, es) result(string)
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(in) :: n
    integer, intent(out) :: iostat
    type(error_stack), intent(inout) :: es
    character(len=n) :: string

    type(buffer_t), pointer :: cb
    integer :: offset, n_left, n_held
    character, pointer :: temp(:)

    if (size(fb%f(1)%next_chars)>0) then
      if (n<size(fb%f(1)%next_chars)) then
        string = str_vs(fb%f(1)%next_chars(:n))
        temp => vs_vs_alloc(fb%f(1)%next_chars(n+1:))
        deallocate(fb%f(1)%next_chars)
        fb%f(1)%next_chars => temp
        iostat = 0
        return
      endif
      offset = size(fb%f(1)%next_chars)
      string(:offset) = str_vs(fb%f(1)%next_chars)
      deallocate(fb%f(1)%next_chars)
      fb%f(1)%next_chars => vs_str_alloc("")
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
      string(offset+1:) = get_chars_from_file(fb%f(1), n_left, iostat, es)
      if (iostat/=0) return ! EOF or Error.
    endif

  end function get_characters

  function get_characters_from_file(f, n, iostat, es) result(string)
    type(xml_source_t), intent(inout) :: f
    integer, intent(in) :: n
    integer, intent(out) :: iostat
    type(error_stack), intent(inout) :: es
    character(len=n) :: string

    integer :: offset, n_left
    character, pointer :: temp(:)

    if (size(f%next_chars)>0) then
      if (n<size(f%next_chars)) then
        string = str_vs(f%next_chars(:n))
        temp => vs_vs_alloc(f%next_chars(n+1:))
        deallocate(f%next_chars)
        f%next_chars => temp
        iostat = 0
        return
      endif
      offset = size(f%next_chars)
      string(:offset) = str_vs(f%next_chars)
      deallocate(f%next_chars)
      f%next_chars => vs_str_alloc("")
    else
      offset = 0
    endif
    n_left = n - offset
    if (n_left==0) then
      iostat = 0
      return
    endif

    string(offset+1:) = get_chars_from_file(f, n_left, iostat, es)

  end function get_characters_from_file


  function get_chars_from_file(f, n, iostat, es) result(string)
    type(xml_source_t), intent(inout) :: f
    integer, intent(in) :: n
    integer, intent(out) :: iostat
    type(error_stack), intent(inout) :: es
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
        c = read_single_char(f, iostat)
        if (iostat==io_eor) then
          c = achar(13)
          iostat = 0
        elseif (iostat/=0) then
          ! FIXME do something with the characters we have read.
          return
        endif
      endif
      if (.not.isLegalChar(c, f%xml_version)) then
        call add_error(es, "Illegal character found at " &
          //str_vs(f%filename)//":"//f%line//":"//f%col)
      endif
      if (c==achar(10)) then
        c2 = read_single_char(f, iostat)
        f%col = f%col + 1
        if (iostat==io_eof) then
          ! the file has just ended on a single CR. Report is as a LF.
          ! Ignore the eof just now, it'll be picked up if we need to 
          ! perform another read.
          iostat = 0
          c = achar(13)
          f%line = f%line + 1
          f%col = 0
        elseif (iostat==io_eor.or.c2==achar(13)) then
          ! (We pretend all ends of lines are LF)
          ! We can drop the CR
          c = c2
        elseif (iostat/=0) then
          call add_error(es, "Error reading "//str_vs(f%filename))
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
      allocate(f%next_chars(1))
      f%next_chars = c2
    endif
  end function get_chars_from_file

  function read_single_char(f, iostat) result(c)
    type(xml_source_t), intent(inout) :: f
    integer, intent(out) :: iostat
    character :: c

    iostat = 0
    if (f%lun==-1) then
      f%input_string%pos = f%input_string%pos + 1
      if (f%input_string%pos>size(f%input_string%s)) then
        iostat = io_eof
      else
        c = f%input_string%s(f%input_string%pos)
      endif
    else
      read (unit=f%lun, iostat=iostat, advance="no", fmt="(a1)") c
    endif
  end function read_single_char

  function line(fb) result(n)
    type(file_buffer_t), intent(in) :: fb
    integer :: n

    n = fb%f(1)%line
  end function line


  function column(fb) result(n)
    type(file_buffer_t), intent(in) :: fb
    integer :: n

    n = fb%f(1)%col
  end function column

  subroutine parse_xml_declaration(f, iostat, es, standalone)
    type(xml_source_t), intent(inout) :: f
    integer, intent(out) :: iostat
    type(error_stack), intent(inout) :: es
    logical, intent(out), optional :: standalone

    integer :: parse_state, xd_par
    character :: c, q
    character, pointer :: ch(:), ch2(:)

    integer, parameter :: XD_0      = 0
    integer, parameter :: XD_START  = 1
    integer, parameter :: XD_TARGET = 2
    integer, parameter :: XD_MISC   = 3
    integer, parameter :: XD_PA     = 4
    integer, parameter :: XD_EQ     = 5
    integer, parameter :: XD_QUOTE  = 6
    integer, parameter :: XD_PV     = 7
    integer, parameter :: XD_END    = 8
    integer, parameter :: XD_SPACE  = 9

    integer, parameter :: xd_nothing = 0
    integer, parameter :: xd_version = 1
    integer, parameter :: xd_encoding = 2
    integer, parameter :: xd_standalone = 3

    f%xml_version = XML1_0
    f%encoding => vs_str_alloc("utf-8")
    if (present(standalone)) standalone = .false.

    parse_state = XD_0
    xd_par = xd_nothing
    do

      c = get_characters_from_file(f, 1, iostat, es)
      if (iostat/=0) then
        return
      endif

      select case (parse_state)

      case (XD_0)
        if (c=="<") then
          parse_state = XD_START
        else
          call push_file_chars(f, c)
          return
        endif

      case (XD_START)
        if (c=="?") then
          parse_state = XD_TARGET
          ch => vs_str_alloc("")
        else
          call push_file_chars(f, "<"//c)
          return
        endif

      case (XD_TARGET)
        if (isXML1_0_NameChar(c)) then
          ch2 => vs_str_alloc(str_vs(ch)//c)
          deallocate(ch)
          ch => ch2
        elseif (verify(c, XML_WHITESPACE)==0 &
          .and.str_vs(ch)=="xml") then
          deallocate(ch)
          parse_state = XD_SPACE
        else
          call push_file_chars(f, "<?"//str_vs(ch)//c)
          deallocate(ch)
          return
        endif

      case (XD_SPACE)
        if (verify(c, XML_WHITESPACE)==0) then
          parse_state = XD_MISC
        else
          call add_error(es, &
            "Missing space in XML declaration")
        endif

      case (XD_MISC)
        if (c=="?") then
          parse_state = XD_END
        elseif (isXML1_0_NameChar(c)) then
          ch => vs_str_alloc(c)
          parse_state = XD_PA
        elseif (verify(c, XML_WHITESPACE)>0) then
          call add_error(es, &
            "Unexpected character in XML declaration")
        endif

      case (XD_PA)
        if (isXML1_0_NameChar(c)) then
          ch2 => vs_str_alloc(str_vs(ch)//c)
          deallocate(ch)
          ch => ch2
        elseif (verify(c, XML_WHITESPACE//"=")==0) then
          select case (str_vs(ch))

          case ("version")
            select case (xd_par)
            case (xd_nothing)
              xd_par = xd_version
            case default
              call add_error(es, &
                "Cannot specify version twice in XML declaration")
            end select

          case ("encoding")
            select case (xd_par)
            case (xd_nothing)
              call add_error(es, &
                "Must specify version before encoding in XML declaration")
            case (xd_version)
              xd_par = xd_encoding
            case (xd_encoding)
              call add_error(es, &
                "Cannot specify encoding twice in XML declaration")
            case (xd_standalone)
              call add_error(es, &
                "Cannot specify encoding after standalone in XML declaration")
            end select

          case ("standalone")
            if (.not.present(standalone)) &
              call add_error(es, &
              "Cannot specify standalone in text declaration")
            select case (xd_par)
            case (xd_nothing)
              call add_error(es, &
                "Must specify version before standalone in XML declaration")
            case (xd_version, xd_encoding)
              xd_par = xd_standalone
            case (xd_standalone)
              call add_error(es, &
                "Cannot specify standalone twice in XML declaration")
            end select

          case default
            call add_error(es, &
              "Unknown parameter "//str_vs(ch)//" in XML declaration, "//&
              "expecting version, encoding or standalone")

          end select

          deallocate(ch)
          if (c=="=") then
            parse_state = XD_QUOTE
          else
            parse_state = XD_EQ
          endif
        else
          call add_error(es, &
            "Unexpected character found in XML declaration")
        endif

      case (XD_EQ)
        if (c=="=") then
          parse_state = XD_QUOTE
        elseif (verify(c, XML_WHITESPACE)>0) then
          call add_error(es, &
            "Unexpected character found in XML declaration; expecting ""=""")
        endif

      case (XD_QUOTE)
        if (verify(c, "'""")==0) then
          q = c
          parse_state = XD_PV
          ch => vs_str_alloc("")
        elseif (verify(c, XML_WHITESPACE)>0) then
          call add_error(es, &
            "Unexpected character found in XML declaration; expecting "" or '")
        endif

      case (XD_PV)
        if (c==q) then
          select case (xd_par)
          case (xd_version)
            if (str_vs(ch)=="1.0") then
              f%xml_version = XML1_0
              deallocate(ch)
            elseif (str_vs(ch)=="1.1") then
              f%xml_version = XML1_1
              deallocate(ch)
            else
              call add_error(es, &
                "Unknown version number "//str_vs(ch)//" found in XML declaration; expecting 1.0 or 1.1")
            endif
          case (xd_encoding)
            if (size(ch)==0) then
              call add_error(es, &
                "Empty value for encoding not allowed in XML declaration")
            elseif (size(ch)==1.and.verify(ch(1), XML_INITIALENCODINGCHARS)>0) then
              call add_error(es, &
                "Invalid encoding found in XML declaration; illegal characters in encoding name")
            elseif (size(ch)>1.and. &
              (verify(ch(1), XML_INITIALENCODINGCHARS)>0 &
              .or.verify(str_vs(ch(2:)), XML_ENCODINGCHARS)>0)) then
              call add_error(es, &
                "Invalid encoding found in XML declaration; illegal characters in encoding name")
            elseif (.not.allowed_encoding(str_vs(ch))) then
              call add_error(es, "Unknown character encoding in XML declaration")
            else
              f%encoding => ch
            endif
          case (xd_standalone)
            if (str_vs(ch)=="yes") then
              standalone = .true.
              deallocate(ch)
            elseif (str_vs(ch)=="no") then
              standalone = .false.
              deallocate(ch)
            else
              call add_error(es, &
                "Invalid value for standalone found in XML declaration; expecting yes or no")

            endif
          end select
          parse_state = XD_SPACE
        else
          ch2 => vs_str_alloc(str_vs(ch)//c)
          deallocate(ch)
          ch => ch2
        endif

      case (XD_END)
        if (c==">") then
          exit
        else
          call add_error(es, &
            "Unexpected character found in XML declaration; expecting >")        
        endif

      end select

      if (in_error(es)) then
        if (associated(ch)) deallocate(ch)
        exit
      endif

    end do

  end subroutine parse_xml_declaration

end module m_sax_reader
