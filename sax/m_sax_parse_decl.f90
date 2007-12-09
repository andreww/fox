module m_sax_parse_decl

  use m_common_array_str, only: vs_str, str_vs, vs_str_alloc
  use m_common_charset, only: XML_WHITESPACE, &
    XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML1_0, XML1_1
  use m_common_error, only: ERR_WARNING, add_error, in_error

  use m_sax_reader, only: file_buffer_t, rewind_file, &
    read_char, read_chars, push_chars
  use m_sax_types ! everything, really

  implicit none
  private

  public :: parse_xml_declaration

contains

  subroutine parse_xml_declaration(fx, fb, iostat)
    type(sax_parser_t), intent(inout) :: fx
    type(file_buffer_t), intent(inout) :: fb
    integer, intent(out) :: iostat
    ! Need to do this with read_* fns, not get_*
    ! FIXME below all read_chars should be error-checked.
    integer :: i
    character(len=*), parameter :: version="version", encoding="encoding", standalone="standalone"
    character :: c
    character(len=5) :: xml = "<?xml"
    character, allocatable :: ch(:)
    ! default values ...
    fx%xds%xml_version = XML1_0
    allocate(fx%xds%encoding(5))
    fx%xds%encoding = vs_str("UTF-8")
    fx%xds%standalone = .false.
    do i = 1, 5
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=xml(i:i)) then
        call rewind_file(fb)
        return
      endif
    enddo
    c = read_char(fb, iostat); if (iostat/=0) return
    if (verify(c,XML_WHITESPACE)/=0) then
      call rewind_file(fb)
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
