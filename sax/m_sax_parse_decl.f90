module m_sax_parse_decl

  use m_common_array_str, only: str_vs, vs_str_alloc
  use m_common_charset, only: XML_WHITESPACE, &
    XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML1_0, XML1_1, isXML1_0_NameChar
  use m_common_error, only: add_error, in_error

  use m_sax_reader, only: file_buffer_t, get_characters, push_chars
  use m_sax_types, only: sax_parser_t

  implicit none
  private

  public :: parse_xml_declaration

contains

  subroutine parse_xml_declaration(fx, fb, iostat)
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

    fx%xds%xml_version = XML1_0
    fx%xds%encoding => vs_str_alloc("utf-8")
    fx%xds%standalone = .false.

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
          elseif (verify(c, XML_WHITESPACE)==0 &
               .and.str_vs(ch)=="xml") then
             deallocate(ch)
             parse_state = XD_MISC
          else
             call push_chars(fb, "<?"//str_vs(ch)//c)
             deallocate(ch)
             return
          endif

       case (XD_MISC)
          if (c=="?") then
             parse_state = XD_END
          elseif (isXML1_0_NameChar(c)) then
             ch => vs_str_alloc(c)
             parse_state = XD_PA
          elseif (verify(c, XML_WHITESPACE)>0) then
             call add_error(fx%error_stack, &
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
             if (c=="=") then
                parse_state = XD_QUOTE
             else
                parse_state = XD_EQ
             endif
          else
             call add_error(fx%error_stack, &
                  "Unexpected character found in XML declaration")
          endif
          
       case (XD_EQ)
          if (c=="=") then
             parse_state = XD_QUOTE
          elseif (verify(c, XML_WHITESPACE)>0) then
             call add_error(fx%error_stack, &
                  "Unexpected character found in XML declaration; expecting ""=""")
          endif

       case (XD_QUOTE)
          if (verify(c, "'""")==0) then
             q = c
             parse_state = XD_PV
             ch => vs_str_alloc("")
          elseif (verify(c, XML_WHITESPACE)>0) then
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
                elseif (size(ch)==1.and.verify(ch(1), XML_INITIALENCODINGCHARS)>0) then
                   call add_error(fx%error_stack, &
                        "Invalid encoding found in XML declaration; illegal characters in encoding name")
                elseif (size(ch)>1.and. &
                     (verify(ch(1), XML_INITIALENCODINGCHARS)>0 &
                     .or.verify(str_vs(ch(2:)), XML_ENCODINGCHARS)>0)) then
                   call add_error(fx%error_stack, &
                        "Invalid encoding found in XML declaration; illegal characters in encoding name")
                else
                   deallocate(fx%xds%encoding)
                   fx%xds%encoding => ch
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

  end subroutine parse_xml_declaration

end module m_sax_parse_decl
