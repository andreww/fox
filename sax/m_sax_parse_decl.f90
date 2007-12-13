module m_sax_parse_decl

  use m_common_array_str, only: vs_str, str_vs, vs_str_alloc
  use m_common_charset, only: XML_WHITESPACE, &
    XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML1_0, XML1_1, isXML1_0_NameChar
  use m_common_error, only: ERR_WARNING, add_error, in_error

  use m_sax_reader, only: file_buffer_t, get_characters, &
    read_char, read_chars, push_chars
  use m_sax_types, only: sax_parser_t

  implicit none
  private

  public :: parse_xml_declaration

contains

  subroutine parse_xml_declaration2(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat

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

    integer, parameter :: xd_nothing = 0
    integer, parameter :: xd_version = 1
    integer, parameter :: xd_encoding = 2
    integer, parameter :: xd_standalone = 3

    parse_state = XD_0
    xd_par = xd_nothing
    do

       c = get_characters(fb, 1, iostat, fx%error_stack)
       if (iostat/=0) then
          return
       endif

       select case (parse_state)

       case (XD_0)
          if (c=="<") then
             parse_state = XD_START
          else
             call push_chars(fb, c)
             return
          endif

       case (XD_START)
          if (c=="?") then
             parse_state = XD_TARGET
             ch => vs_str_alloc("")
          else
             call push_chars(fb, "<"//c)
             return
          endif

       case (XD_TARGET)
          if (isXML1_0_NameChar(c)) then
             ch2 => vs_str_alloc(str_vs(ch)//c)
             deallocate(ch)
             ch => ch2
          elseif (isXML1_0_NameChar(c) &
               .and.str_vs(ch)=="xml") then
             deallocate(ch)
             parse_state = XD_MISC
          else
             call push_chars(fb, "<?"//str_vs(ch))
             deallocate(ch)
             return
          endif

       case (XD_MISC)
          if (c=="?") then
             parse_state = XD_END
          elseif (isXML1_0_NameChar(c)) then
             ch => vs_str_alloc(c)
             parse_state = XD_PA
          elseif (verify(c, XML_WHITESPACE)==0) then
             call add_error(fx%error_stack, &
                  "Unexpected character in XML declaration")
          endif

       case (XD_PA)
          if (isXML1_0_NameChar(c)) then
             ch2 => vs_str_alloc(str_vs(ch)//c)
             deallocate(ch)
             ch => ch2
          elseif (verify(c, XML_WHITESPACE)>0) then
             select case (str_vs(ch))

             case ("version")
                select case (xd_par)
                case (xd_nothing)
                   xd_par = xd_version
                case default
                   call add_error(fx%error_stack, &
                        "Cannot specify version twice in XML declaration")
                end select

             case ("encoding")
                select case (xd_par)
                case (xd_nothing)
                   call add_error(fx%error_stack, &
                        "Must specify version before encoding in XML declaration")
                case (xd_version)
                   xd_par = xd_encoding
                case (xd_encoding)
                   call add_error(fx%error_stack, &
                        "Cannot specify encoding twice in XML declaration")
                case (xd_standalone)
                   call add_error(fx%error_stack, &
                        "Cannot specify encoding after standalone in XML declaration")
                end select

             case ("standalone")
                select case (xd_par)
                case (xd_nothing)
                   call add_error(fx%error_stack, &
                        "Must specify version before standalone in XML declaration")
                case (xd_version, xd_encoding)
                   xd_par = xd_standalone
                case (xd_standalone)
                   call add_error(fx%error_stack, &
                        "Cannot specify standalone twice in XML declaration")
                end select

             case default
                call add_error(fx%error_stack, &
                     "Unknown parameter "//str_vs(ch)//" in XML declaration, "//&
                     "expecting version, encoding or standalone")

             end select

             deallocate(ch)
             parse_state = XD_EQ
          else
             call add_error(fx%error_stack, &
                  "Unexpected character found in XML declaration")
          endif
          
       case (XD_EQ)
          if (c=="=") then
             parse_state = XD_QUOTE
          elseif (verify(c, XML_WHITESPACE)==0) then
             call add_error(fx%error_stack, &
                  "Unexpected character found in XML declaration; expecting ""=""")
          endif

       case (XD_QUOTE)
          if (verify(c, "'""")>0) then
             q = c
             parse_state = XD_PV
             ch => vs_str_alloc("")
          else
             call add_error(fx%error_stack, &
                  "Unexpected character found in XML declaration; expecting "" or '")
          endif

       case (XD_PV)
          if (c==q) then
             select case (xd_par)
             case (xd_version)
                if (str_vs(ch)=="1.0") then
                   fx%xds%xml_version = XML1_0
                   deallocate(ch)
                elseif (str_vs(ch)=="1.1") then
                   fx%xds%xml_version = XML1_1
                   deallocate(ch)
                else
                   call add_error(fx%error_stack, &
                        "Unknown version number "//str_vs(ch)//" found in XML declaration; expecting 1.0 or 1.1")
                endif
             case (xd_encoding)
                if (size(ch)==0) then
                   call add_error(fx%error_stack, &
                        "Empty value for encoding not allowed in XML declaration")
                elseif (size(ch)==1.and.verify(ch(1), XML_INITIALENCODINGCHARS)==0) then
                   call add_error(fx%error_stack, &
                        "Invalid encoding found in XML declaration; illegal characters in encoding name")
                elseif (size(ch)>1.and. &
                     (verify(ch(1), XML_INITIALENCODINGCHARS)==0 &
                     .or.verify(str_vs(ch(2:)), XML_ENCODINGCHARS)==0)) then
                   call add_error(fx%error_stack, &
                        "Invalid encoding found in XML declaration; illegal characters in encoding name")
                else
                   fx%xds%encoding => ch
                   ! Check we know encoding
                endif
             case (xd_standalone)
                if (str_vs(ch)=="yes") then
                   fx%xds%standalone = .true.
                   deallocate(ch)
                elseif (str_vs(ch)=="no") then
                   fx%xds%standalone = .false.
                   deallocate(ch)
                else
                   call add_error(fx%error_stack, &
                        "Invalid value for standalone found in XML declaration; expecting yes or no")

                endif
             end select
             deallocate(ch)
             parse_state = XD_MISC
          else
             ch2 => vs_str_alloc(str_vs(ch)//c)
             deallocate(ch)
             ch => ch2
          endif

       case (XD_END)
          if (c==">") then
             exit
          else
             call add_error(fx%error_stack, &
                  "Unexpected character found in XML declaration; expecting >")        
          endif

       end select

       if (in_error(fx%error_stack)) then
          if (associated(ch)) deallocate(ch)
          exit
       endif
       
    end do

  end subroutine parse_xml_declaration2

  subroutine parse_xml_declaration(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat
    ! Need to do this with read_* fns, not get_*
    ! FIXME below all read_chars should be error-checked.
    integer :: i
    character(len=*), parameter :: version="version", encoding="encoding", standalone="standalone"
    character :: c
    character(len=5) :: start
    character, allocatable :: ch(:)
    ! default values ...
    fx%xds%xml_version = XML1_0
    allocate(fx%xds%encoding(5))
    fx%xds%encoding = vs_str("UTF-8")
    fx%xds%standalone = .false.
    start = get_characters(fb, 5, iostat, fx%error_stack)
    if (iostat/=0) return
    if (start/="<?xml") then
      call push_chars(fb, start)
      return
    endif
    c = get_characters(fb, 1, iostat, fx%error_stack)
    if (iostat/=0) return
    if (verify(c,XML_WHITESPACE)/=0) then
      call push_chars(fb, c)
      return
    endif
    do while (verify(c,XML_WHITESPACE)==0)
      c = read_char(fb, iostat); if (iostat/=0) return
    enddo
    call push_chars(fb, c)
    allocate(ch(7))
    ch = vs_str(read_chars(fb, 7, iostat)); if (iostat/=0) return
    if (str_vs(ch)/="version") then
      deallocate(ch)
      call add_error(fx%error_stack, "Expecting XML version"); return
    endif
    deallocate(ch)
    call check_version
    if (iostat/=0) return
    c = read_char(fb, iostat); if (iostat/=0) return
    if (verify(c,XML_WHITESPACE)/=0.and.c/='?') then
      call add_error(fx%error_stack, "Missing whitespace in XML declaration");return
    endif
    do while (verify(c,XML_WHITESPACE)==0)
      c = read_char(fb, iostat); if (iostat/=0) return
    enddo
    if (c=='?') then
      c = read_char(fb, iostat); if (iostat/=0) return
      ! FIXME read_char io_eor handling
      if (c/='>') then
        call add_error(fx%error_stack, "Expecting > to end XML declaration"); return
      endif
      return
    endif
    call push_chars(fb, c)
    allocate(ch(8))
    ch = vs_str(read_chars(fb, 8, iostat)); if (iostat/=0) return
    if (str_vs(ch)/="encoding") then
      call push_chars(fb, str_vs(ch))
      deallocate(ch)
      allocate(ch(10))
      ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="standalone") then
        deallocate(ch)
        call add_error(fx%error_stack, "Expecting XML encoding or standalone declaration"); return
      endif
      deallocate(ch)
      call check_standalone
      if (iostat/=0.or.in_error(fx%error_stack)) return
    else
      deallocate(ch)
      call check_encoding
      if (iostat/=0.or.in_error(fx%error_stack)) return
      c = read_char(fb, iostat); if (iostat/=0) return
      if (verify(c,XML_WHITESPACE)/=0.and.c/='?') then
        call add_error(fx%error_stack, "Missing whitespace in XML declaration");return
      endif
      do while (verify(c,XML_WHITESPACE)==0)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c=='?') then
        c = read_char(fb, iostat); if (iostat/=0) return
        ! FIXME read_char io_eor handling
        if (c/='>') then
          call add_error(fx%error_stack, "Expecting > to end XML declaration"); return
        endif
        return
      endif
      call push_chars(fb, c)
      allocate(ch(10))
      ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="standalone") then
        deallocate(ch)
        call add_error(fx%error_stack, "Expecting XML encoding or standalone declaration"); return
      endif
      deallocate(ch)
      call check_standalone
      if (iostat/=0.or.in_error(fx%error_stack)) return
    endif
    c = read_char(fb, iostat); if (iostat/=0) return
    if (verify(c,XML_WHITESPACE)/=0.and.c/='?') then
      call add_error(fx%error_stack, "Missing whitespace in XML declaration");return
    endif
    do while (verify(c,XML_WHITESPACE)==0)
      c = read_char(fb, iostat); if (iostat/=0) return
    enddo
    if (c=='?') then
      c = read_char(fb, iostat); if (iostat/=0) return
      ! FIXME read_char io_eor handling
      if (c/='>') then
        call add_error(fx%error_stack, "Expecting > to end XML declaration"); return
      endif
    else
      call add_error(fx%error_stack, "Expecting '?>' to end XML declaration"); return
    endif

  contains

    subroutine check_version
      character :: c, quotechar
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (verify(c,XML_WHITESPACE)==0)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="=") then
        call add_error(fx%error_stack, "Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (verify(c,XML_WHITESPACE)==0)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call add_error(fx%error_stack, "Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1") then
        call add_error(fx%error_stack, "Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=".") then
        call add_error(fx%error_stack, "Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1".and.c/="0") then
        call add_error(fx%error_stack, "Unknown XML version"); return
      endif
      if (c=="1") then
        fx%xds%xml_version = XML1_1
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=quotechar) then
        call add_error(fx%error_stack, "Expecting "//quotechar); return
      endif
    end subroutine check_version

    subroutine check_encoding
      character :: c, quotechar
      character, dimension(:), pointer :: buf, tempbuf
      integer :: i
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (verify(c,XML_WHITESPACE)==0)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="=") then
        call add_error(fx%error_stack, "Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (verify(c,XML_WHITESPACE)==0)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call add_error(fx%error_stack, "Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (verify(c,XML_INITIALENCODINGCHARS)/=0) then
        call add_error(fx%error_stack, "Illegal character at start of encoding declaration."); return
      endif
      i = 1
      buf => vs_str_alloc(c)
      c = read_char(fb, iostat)
      if (iostat/=0) then
        deallocate(buf)
        return
      endif
      do while (verify(c,XML_ENCODINGCHARS)==0)
        tempbuf => buf
        i = i+1
        allocate(buf(i))
        buf(:i-1) = tempbuf
        deallocate(tempbuf)
        buf(i) = c
        c = read_char(fb, iostat)
        if (iostat/=0) then
          deallocate(buf)
          return
        endif
      enddo
      if (c/=quotechar) then
        call add_error(fx%error_stack, "Illegal character in XML encoding declaration; expecting "//quotechar); 
        deallocate(buf)
        return
      endif
      deallocate(fx%xds%encoding)
      fx%xds%encoding => buf
    end subroutine check_encoding

    subroutine check_standalone
      character :: c, quotechar
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (verify(c,XML_WHITESPACE)==0)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="=") then
        call add_error(fx%error_stack, "Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (verify(c,XML_WHITESPACE)==0)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call add_error(fx%error_stack, "Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c=="y") then
        c = read_char(fb, iostat); if (iostat/=0) return
        if (c=="e") then
          c = read_char(fb, iostat); if (iostat/=0) return
          if (c=="s") then
            fx%xds%standalone = .true.
          else
            call add_error(fx%error_stack, "standalone accepts only 'yes' or 'no'"); return
          endif
        else
          call add_error(fx%error_stack, "standalone accepts only 'yes' or 'no'"); return
        endif
      elseif (c=="n") then
        c = read_char(fb, iostat); if (iostat/=0) return
        if (c=="o") then
          fx%xds%standalone = .false.
        else
          call add_error(fx%error_stack, "standalone accepts only 'yes' or 'no'"); return
        endif
      else
        call add_error(fx%error_stack, "standalone accepts only 'yes' or 'no'"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=quotechar) then
        call add_error(fx%error_stack, "Expecting "" or '"); return
      endif
      fx%xds%standalone_declared = .true.

    end subroutine check_standalone

  end subroutine parse_xml_declaration

end module m_sax_parse_decl
