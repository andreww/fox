module m_sax_fsm

  use m_common_attrs, only: dictionary_t, add_key_to_dict, add_value_to_dict, &
       init_dict, reset_dict, destroy_dict
  use m_common_array_str, only: vs_str, str_vs
  use m_common_buffer, only: buffer_t, buffer_to_chararray, len, str, &
       buffer_nearly_full, add_to_buffer, reset_buffer
  use m_common_error, only: FoX_warning, FoX_error, FoX_fatal
  use m_common_charset, only: XML1_0, XML1_1, XML1_0_INITIALNAMECHARS, &
    XML1_1_INITIALNAMECHARS, XML_INITIALENCODINGCHARS, XML_ENCODINGCHARS, &
    XML_WHITESPACE, XML1_0_NAMECHARS, XML1_1_NAMECHARS
  use m_common_charset, only: validchars, initialnamechars, namechars, &
       whitespace, uppercase, operator(.in.)
  use m_common_elstack, only: elstack_t, init_elstack, reset_elstack, destroy_elstack, is_empty
  use m_common_io, only: io_eof
  use m_common_namespaces, only: namespaceDictionary, initNamespaceDictionary, &
       destroyNamespaceDictionary
  
  use m_common_entities, only: init_entity_list, destroy_entity_list, reset_entity_list
  use m_common_entities, only: entity_list, entity_filter_text_len, entity_filter_text
  use m_common_entities, only: is_unparsed_entity

  use m_sax_reader, only: file_buffer_t, open_file, read_char, read_chars, &
    get_characters, get_next_character_discarding_whitespace, &
    get_characters_until_not_one_of, get_characters_until_one_of, &
    get_characters_until_all_of, put_characters, &
    len_namebuffer, retrieve_namebuffer
  
  implicit none
  private

  public :: init_fsm, reset_fsm, destroy_fsm, evolve_fsm

  ! State parameters
  integer, parameter ::  ERROR = -1
  integer, parameter ::  INIT = 1         
  integer, parameter ::  START_TAG_MARKER = 2
  integer, parameter ::  END_TAG_MARKER = 3
  integer, parameter ::  IN_NAME = 4
  integer, parameter ::  WHITESPACE_IN_TAG = 5
  integer, parameter ::  IN_PCDATA = 6
  integer, parameter ::  SINGLETAG_MARKER = 7
  integer, parameter ::  CLOSINGTAG_MARKER = 8
  integer, parameter ::  IN_COMMENT = 9
  integer, parameter ::  BANG = 17
  integer, parameter ::  BANG_HYPHEN = 18
  integer, parameter ::  ONE_HYPHEN = 19
  integer, parameter ::  TWO_HYPHEN = 20
  integer, parameter ::  IN_ATTS = 21
  integer, parameter ::  START_PI = 22
  integer, parameter ::  IN_DTD = 23
  integer, parameter ::  IN_CDATA_SECTION = 24
  integer, parameter ::  ONE_BRACKET = 25
  integer, parameter ::  TWO_BRACKET = 26
  integer, parameter ::  CDATA_PREAMBLE = 27
  integer, parameter ::  IN_PI_TARGET = 28
  integer, parameter ::  WHITESPACE_OUTSIDE_BODY = 29
  integer, parameter ::  WHITESPACE_IN_CLOSING_TAG = 30
  integer, parameter ::  QUESTION_MARK_IN_TARGET = 32
  integer, parameter ::  QUESTION_MARK_IN_CONTENT = 33

  ! Context parameters
  integer, parameter ::  OPENING_TAG  = 100
  integer, parameter ::  CLOSING_TAG  = 110
  integer, parameter ::  SINGLE_TAG   = 120
  integer, parameter ::  COMMENT_TAG  = 130
  integer, parameter ::  PI_TAG  = 140
  integer, parameter ::  DTD_TAG  = 150
  integer, parameter ::  CDATA_SECTION_TAG  = 160
!  integer, parameter ::  NULL_CONTEXT          = 200

  ! State ...
  integer, parameter :: ST_NULL = -1
  integer, parameter :: ST_INIT = 0
  integer, parameter :: ST_MISC = 1
  integer, parameter :: ST_START_TAG = 2
  integer, parameter :: ST_IN_TAG = 3
  integer, parameter :: ST_CDATA = 4
  integer, parameter :: ST_CLOSE_TAG = 5

  
  ! Context
  integer, parameter :: CTXT_NULL = -1
  integer, parameter :: CTXT_INIT = 0
  integer, parameter :: CTXT_BEFORE_DTD = 1
  integer, parameter :: CTXT_IN_DTD = 2
  integer, parameter :: CTXT_AFTER_DTD = 3
  integer, parameter :: CTXT_MARKUP = 4
  integer, parameter :: CTXT_CDATA = 5
  integer, parameter :: CTXT_AFTER_ELEMENT = 6

  ! Signal parameters
  integer, parameter ::  QUIET             = 1000
  integer, parameter ::  END_OF_TAG        = 1100
  integer, parameter ::  CHUNK_OF_PCDATA   = 1200
  integer, parameter ::  END_OF_DOCUMENT   = 1300
  integer, parameter ::  EXCEPTION         = 1500

  type, public :: fsm_t
    ! Contains information about the "finite state machine"
    integer                          :: state = ST_NULL
    integer                          :: context = CTXT_NULL
    integer                          :: nbrackets
    integer                          :: nlts
    logical                          :: in_quotes
    character(len=1)                 :: quote_char
    type(buffer_t)                   :: buffer
    character, dimension(:), pointer :: pcdata
    character, dimension(:), pointer :: element_name
    type(dictionary_t)               :: attributes
    type(namespaceDictionary)        :: nsDict
    type(elstack_t)                  :: element_stack
    logical                          :: root_element_seen
    character, dimension(:), pointer :: root_element_name
    type(entity_list)                :: entities
    character, dimension(:), pointer :: action
    logical                          :: debug
    logical                          :: xml_decl_ok
    integer                          :: xml_version
    character, dimension(:), pointer :: encoding
    logical                          :: standalone
  end type fsm_t

  public :: OPENING_TAG
  public :: CLOSING_TAG
  public :: SINGLE_TAG 
  public :: COMMENT_TAG
  public :: PI_TAG
  public :: DTD_TAG
  public :: CDATA_SECTION_TAG

  public :: QUIET
  public :: END_OF_TAG
  public :: CHUNK_OF_PCDATA
  public :: END_OF_DOCUMENT
  public :: EXCEPTION
  
contains

  subroutine init_fsm(fx) 
    type(fsm_t), intent(inout)   :: fx
    
    fx%state = ST_INIT
    fx%context = CTXT_INIT
    call init_elstack(fx%element_stack)
    fx%root_element_seen = .false.
    fx%debug = .false.
    allocate(fx%action(0))
    ! We initialise to XML-1.0 by default, and reinitialise later if necessary
    fx%xml_version = XML1_0
    allocate(fx%encoding(5))
    fx%encoding = vs_str("UTF-8")
    fx%standalone = .false.
    !call reset_buffer(fx%buffer)
    nullify(fx%element_name)
    nullify(fx%pcdata)
    nullify(fx%root_element_name)
    call init_dict(fx%attributes)
    call initNamespaceDictionary(fx%nsDict)
    call init_entity_list(fx%entities, PE=.false.)
    fx%xml_decl_ok = .true.
  end subroutine init_fsm

  subroutine reset_fsm(fx) 
    type(fsm_t), intent(inout)   :: fx
    
    fx%state = ST_INIT
    fx%context = CTXT_INIT
    call reset_elstack(fx%element_stack)
    fx%action = ""
    fx%root_element_seen = .false.
    ! We initialise to XML-1.0 by default, and reinitialise later if necessary
    fx%xml_version = XML1_0
    allocate(fx%encoding(5))
    fx%encoding = vs_str("UTF-8")
    fx%standalone = .false.
    !call reset_buffer(fx%buffer, xml_version=fx%xml_version)
    if (associated(fx%element_name)) deallocate(fx%element_name)
    nullify(fx%element_name)
    if (associated(fx%pcdata)) deallocate(fx%pcdata)
    nullify(fx%pcdata)
    if (associated(fx%root_element_name)) deallocate(fx%root_element_name)
    nullify(fx%root_element_name)
    call reset_dict(fx%attributes)
    call destroyNamespaceDictionary(fx%nsDict)
    call initNamespaceDictionary(fx%nsDict)
    call reset_entity_list(fx%entities)
    fx%xml_decl_ok = .true.
  end subroutine reset_fsm
  
  subroutine destroy_fsm(fx) 
    type(fsm_t), intent(inout)   :: fx
    
    fx%state = ST_NULL
    fx%context = CTXT_NULL
    call destroy_elstack(fx%element_stack)
    deallocate(fx%action)
    fx%root_element_seen = .false.
    ! buffer does not need any deallocation
    if (associated(fx%element_name)) deallocate(fx%element_name)
    if (associated(fx%pcdata)) deallocate(fx%pcdata)
    if (associated(fx%root_element_name)) deallocate(fx%root_element_name)
    deallocate(fx%encoding)
    call destroy_dict(fx%attributes)
    call destroyNamespaceDictionary(fx%nsDict)
    call destroy_entity_list(fx%entities)
  end subroutine destroy_fsm
  
  subroutine evolve_fsm(fx, fb, signal)
    !
    ! Finite-state machine evolution rules for XML parsing.
    !
    type(fsm_t), intent(inout)      :: fx    ! Internal state
    type(file_buffer_t), intent(inout)    :: fb
    integer, intent(out)            :: signal

    character(len=1) :: c
    character, allocatable :: ch(:)
    integer :: iostat
    type(buffer_t) :: tempBuf

    parse_loop: do 

      select case(fx%state)

      case (ST_NULL)
        call FoX_fatal("Internal error: Trying to manipulate uninitialized fsm")  

      case (ST_INIT)
        ! Check for XML declaration - then move onto prologue
        c = read_char(fb, iostat); if (iostat/=0) goto 100
        if (c.in.whitespace) then
          fx%xml_decl_ok = .false.
          c = get_next_character_discarding_whitespace(fb, iostat)
          if (iostat/=0) goto 100
        endif
        if (c == "<") then
          c = read_char(fb, iostat); if (iostat/=0) goto 100
          if (c == "?") then
            ! must do this extra step - if we tried to get 5 chars
            ! before we would fail on a doc containing "<a/>"
            allocate(ch(4))
            ch = vs_str(read_chars(fb, 4, iostat)); if (iostat/=0) goto 100
            if (str_vs(ch(1:3))=='xml'.and.(ch(4).in.XML_WHITESPACE)) then
              deallocate(ch)
              if (fx%xml_decl_ok) then
                call parse_xml_decl
                if (iostat/=0.or.fx%state==ERROR) goto 100
              else
                call parse_error("Whitespace before XML declaration"); goto 100
              endif
            else
              ! This is a PI (or an error) - handle elsewhere.
              call put_characters(fb, str_vs(ch))
              deallocate(ch)
              call parse_action("No XML Declaration found.")
            endif
          else
            ! This is a normal tag (or an error) - handle elsewhere.
            call put_characters(fb, c)
          endif
        else
          call parse_error("Meaningless characters at beginning of XML file."); goto 100
        endif
        fx%state = ST_MISC
        fx%context = CTXT_BEFORE_DTD
        exit


      case (ST_MISC)
        ! context must be either CTXT_BEFORE_DTD or CTXT_BEFORE_ELEMENT
        ! or CTXT_AFTER_ELEMENT
        c = get_next_character_discarding_whitespace(fb, iostat)
        if (iostat/=0) goto 100
        if (c=="<") then
          c = get_characters(fb, 1, iostat)
          if (iostat/=0) goto 100
          if (c=='?') then
            call parse_pi
            !error check
          elseif (c=='!') then
            call parse_bang
            !error check
          elseif (c.in.initialNameChars) then !check version
            if (fx%context == CTXT_AFTER_ELEMENT) then
              call parse_error("Cannot have two root elements"); goto 100
            endif
            ! Otherwise we've just found the root element.
            call put_characters(fb, c)
            fx%state = ST_START_TAG
          else
            call parse_error("Illegal character found after <")
          end if
        elseif (fx%context == CTXT_AFTER_ELEMENT) then
          call parse_error("Illegal character found after root element.")
        elseif (fx%context == CTXT_AFTER_ELEMENT) then
          call parse_error("Illegal character found before root element.")
        endif

!      case (DTD)
        
      case (ST_START_TAG)
        c = get_characters(fb, 1, iostat)
        if (iostat /= 0) goto 100
        if (c .in. initialNameChars) then
          call put_characters(fb, c)
          call get_characters_until_not_one_of(fb, XML1_0_NAMECHARS, iostat)
          if (iostat/=0) goto 100
          allocate(fx%element_name(len_namebuffer(fb)))
          fx%element_name = vs_str(retrieve_namebuffer(fb))
          if (fx%context==CTXT_BEFORE_DTD) then
            allocate(fx%root_element_name(size(fx%element_name)))
            fx%root_element_name = fx%element_name
            fx%context = CTXT_MARKUP
          elseif (fx%context==CTXT_AFTER_DTD) then
            if (str_vs(fx%element_name)/=str_vs(fx%root_element_name)) then
              call parse_error("Root element name does not match DOCTYPE"); goto 100
            endif
            fx%context = CTXT_MARKUP
          elseif (fx%context/=CTXT_CDATA) then
            ! We should never be here
            call FoX_error("Internal error in evolve_fsm, START_TAG")
          endif
          fx%state = ST_IN_TAG
        elseif (c=='/') then
          fx%state = ST_CLOSE_TAG
          cycle
        elseif (c=='?') then
          call parse_pi
          if (iostat/=0.or.fx%state==ERROR) goto 100
        elseif (c=='!') then
          call parse_bang
          if (iostat/=0.or.fx%state==ERROR) goto 100
        else
          call parse_error("Unexpected character after >"); goto 100
        endif

        
      case (ST_CLOSE_TAG)
        c = get_next_character_discarding_whitespace(fb, iostat)
        if (iostat/=0) goto 100
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) goto 100
        if (c == ">") then
          ! Namespace manipulation
          ! callback starttag
          ! callback endtag
          if (is_empty(fx%element_stack)) then
            fx%context = CTXT_AFTER_ELEMENT
            fx%state = ST_MISC
          else
            fx%state = ST_CDATA
          endif
        else
          call parse_error("Unexpected character found - expecting >")
        endif

      case (ST_CDATA)
        ! look along until we hit '&' or '<'.
        ! if we hit an &, then recursively expand & reparse, unless
        ! it's a CharRef or default entity in which case don't expand 
        ! until after reparsing?
        call get_characters_until_one_of(fb, '<&', iostat)
        c = get_characters(fb, 1, iostat) ! Can't fail.
        select case(c)
        case ('<')
          ! expand all character references in buffer
          ! character callback
!          fx%state = STARTING_TAG
          continue
        case ('&')
          call parse_entityreference

        end select

      end select
    enddo parse_loop

100 if (allocated(ch)) deallocate(ch)
    if (iostat==io_eof) then
      call parse_error("Unexpected end of file.")
    elseif (iostat/=0) then
      call parse_error("Unknown file error encountered.")
    endif

    if (fx%state==ERROR) then
      signal = EXCEPTION
    else
      signal = QUIET
    endif

    return

  contains
    subroutine parse_xml_decl
      ! Need to do this with read_* fns, not get_*
      ! FIXME below all read_chars should be error-checked.
      integer :: i
      character(len=*), parameter :: version="version", encoding="encoding", standalone="standalone"
      character :: c, quotation_mark
      character, allocatable :: ch(:)
      c = " "
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      call put_characters(fb, c)
      allocate(ch(7))
      ch = vs_str(read_chars(fb, 7, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="version") then
        deallocate(ch)
        call parse_error("Expecting XML version"); return
      endif
      deallocate(ch)
      call check_version
      if (iostat/=0.or.fx%state==ERROR) return
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c=='?') then
        c = read_char(fb, iostat); if (iostat/=0) return
        ! FIXME read_char io_eor handling
        if (c/='>') then
          call parse_error("Expecting > to end XML declaration"); return
        endif
        return
      endif
      call put_characters(fb, c)
      allocate(ch(8))
      ch = vs_str(read_chars(fb, 8, iostat)); if (iostat/=0) return
      if (str_vs(ch)/="encoding") then
        call put_characters(fb, str_vs(ch))
        deallocate(ch)
        allocate(ch(10))
        ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
        if (str_vs(ch)/="standalone") then
          deallocate(ch)
          call parse_error("Expecting XML encoding or standalone declaration"); return
        endif
        deallocate(ch)
        call check_standalone
        if (iostat/=0.or.fx%state==ERROR) return
      else
        deallocate(ch)
        call check_encoding
        if (iostat/=0.or.fx%state==ERROR) return
        c = read_char(fb, iostat); if (iostat/=0) return
        do while (c.in.XML_WHITESPACE)
          c = read_char(fb, iostat); if (iostat/=0) return
        enddo
        if (c=='?') then
          c = read_char(fb, iostat); if (iostat/=0) return
          ! FIXME read_char io_eor handling
          if (c/='>') then
            call parse_error("Expecting > to end XML declaration"); return
          endif
          return
        endif
        call put_characters(fb, c)
        allocate(ch(10))
        ch = vs_str(read_chars(fb, 10, iostat)); if (iostat/=0) return
        if (str_vs(ch)/="standalone") then
          deallocate(ch)
          call parse_error("Expecting XML encoding or standalone declaration"); return
        endif
        deallocate(ch)
        call check_standalone
        if (iostat/=0.or.fx%state==ERROR) return
        c = read_char(fb, iostat); if (iostat/=0) return
        do while (c.in.XML_WHITESPACE)
          c = read_char(fb, iostat); if (iostat/=0) return
        enddo
        if (c=='?') then
          c = read_char(fb, iostat); if (iostat/=0) return
          ! FIXME read_char io_eor handling
          if (c/='>') then
            call parse_error("Expecting > to end XML declaration"); return
          endif
        endif
      endif

      if (str_vs(fx%encoding)/="UTF-8") then
        call FoX_warning("Unknown character encoding in XML declaration. "//&
          "Assuming you know what you are doing, going ahead anyway.")
      endif

    end subroutine parse_xml_decl
    
    subroutine check_version
      character :: c, quotechar
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="=") then
        call parse_error("Expecting ="); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      do while (c.in.XML_WHITESPACE)
        c = read_char(fb, iostat); if (iostat/=0) return
      enddo
      if (c/="'".and.c/='"') then
        call parse_error("Expecting "" or '"); return
      endif
      quotechar = c
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1") then
        call parse_error("Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=".") then
        call parse_error("Unknown XML version"); return
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/="1".and.c/="0") then
        call parse_error("Unknown XML version"); return
      endif
      if (c=="1") then
        fx%xml_version = XML1_1
      endif
      c = read_char(fb, iostat); if (iostat/=0) return
      if (c/=quotechar) then
        call parse_error("Expecting "//quotechar); return
      endif
    end subroutine check_version

      subroutine check_encoding
        character :: c, quotechar
        character, dimension(:), pointer :: buf, tempbuf
        integer :: i
        c = read_char(fb, iostat); if (iostat/=0) return
        do while (c.in.XML_WHITESPACE)
          c = read_char(fb, iostat); if (iostat/=0) return
        enddo
        if (c/="=") then
          call parse_error("Expecting ="); return
        endif
        c = read_char(fb, iostat); if (iostat/=0) return
        do while (c.in.XML_WHITESPACE)
          c = read_char(fb, iostat); if (iostat/=0) return
        enddo
        if (c/="'".and.c/='"') then
          call parse_error("Expecting "" or '"); return
        endif
        quotechar = c
        c = read_char(fb, iostat); if (iostat/=0) return
        if (.not.(c.in.XML_INITIALENCODINGCHARS)) then
          call parse_error("Illegal character at start of encoding declaration."); return
        endif
        i = 1
        allocate(buf(1))
        buf(1) = c
        c = read_char(fb, iostat)
        if (iostat/=0) then
          deallocate(buf)
          return
        endif
        do while (c.in.XML_ENCODINGCHARS)
          i = i+1
          allocate(tempbuf(i))
          tempbuf(:i-1) = buf
          deallocate(buf)
          tempbuf(i) = c
          buf => tempbuf
          c = read_char(fb, iostat)
          if (iostat/=0) then
            deallocate(buf)
            return
          endif
        enddo
        if (c/=quotechar) then
          call parse_error("Illegal character in XML encoding declaration; expecting "//quotechar); return
        endif
        deallocate(fx%encoding)
        fx%encoding => buf
      end subroutine check_encoding

      subroutine check_standalone
        character :: c, quotechar
        c = read_char(fb, iostat); if (iostat/=0) return
        do while (c.in.XML_WHITESPACE)
          c = read_char(fb, iostat); if (iostat/=0) return
        enddo
        if (c/="=") then
          call parse_error("Expecting ="); return
        endif
        c = read_char(fb, iostat); if (iostat/=0) return
        do while (c.in.XML_WHITESPACE)
          c = read_char(fb, iostat); if (iostat/=0) return
        enddo
        if (c/="'".and.c/='"') then
          call parse_error("Expecting "" or '"); return
        endif
        quotechar = c
        c = read_char(fb, iostat); if (iostat/=0) return
        if (c=="y") then
          c = read_char(fb, iostat); if (iostat/=0) return
          if (c=="e") then
            c = read_char(fb, iostat); if (iostat/=0) return
            if (c=="s") then
              fx%standalone = .true.
            else
              call parse_error("standalone accepts only 'yes' or 'no'"); return
            endif
          else
            call parse_error("standalone accepts only 'yes' or 'no'"); return
          endif
        elseif (c=="n") then
          c = read_char(fb, iostat); if (iostat/=0) return
          if (c=="o") then
            fx%standalone = .false.
          else
            call parse_error("standalone accepts only 'yes' or 'no'"); return
          endif
        else
          call parse_error("standalone accepts only 'yes' or 'no'"); return
        endif
        c = read_char(fb, iostat); if (iostat/=0) return
        if (c/=quotechar) then
          call parse_error("Expecting "" or '"); return
        endif
      end subroutine check_standalone

    subroutine parse_bang()
      allocate(ch(2))
      ch = vs_str(get_characters(fb, 2, iostat))
      if (iostat/=0) return
      if (str_vs(ch) == '--') then 
        call parse_comment()
      elseif (str_vs(ch) == 'DO') then
        deallocate(ch)
        allocate(ch(5))
        if (str_vs(ch) == 'CTYPE') then
          call parse_dtd()
        endif
        deallocate(ch)
        ! but only check for CDATA if we are in character context
      elseif (str_vs(ch) == '[C') then
        deallocate(ch)
        allocate(ch(5))
        if (str_vs(ch) == 'DATA[') then
          call parse_cdata()
        endif
        deallocate(ch)
      else
        call put_characters(fb, str_vs(ch))
        deallocate(ch)
        call parse_error("blah") ! FIXME
      endif
    end subroutine parse_bang

    subroutine parse_pi
      ! need better check here
      c = get_next_character_discarding_whitespace(fb, iostat)
      if (iostat/=0) return
      ! FIXME need to find PI name properly
      allocate(fx%element_name(len(fx%buffer)))
      fx%element_name = buffer_to_chararray(fx%buffer)
      call get_characters_until_all_of(fb, '?>', iostat)
      if (iostat/=0) return
!      PI callback.
!      state = something
      allocate(ch(2))
      ch = get_characters(fb, 2, iostat) ! must succeed
      deallocate(ch)
    end subroutine parse_pi

    subroutine parse_comment
      call get_characters_until_all_of(fb, '--', iostat)
      if (iostat/=0) return
      allocate(ch(3))
      ch = str_vs(get_characters(fb, 3, iostat))
      if (iostat/=0) return
      if (str_vs(ch) == '-->') then
 !       comment callback
        deallocate(ch)
 !       STATE=something
      else
        deallocate(ch)
        call parse_error("Not allowed -- in the middle of a comment.")
      endif

    end subroutine parse_comment

    subroutine parse_cdata
      call get_characters_until_all_of(fb, ']]>', iostat)
      if (iostat/=0) return
!      cdata callback
!      STATE=something
    end subroutine parse_cdata

    subroutine parse_name
      ! get characters until not-a-name-char.
      call get_characters_until_not_one_of(fb, XML1_0_NAMECHARS, iostat)
      if (iostat/=0) return
      if (len_namebuffer(fb) == 0) then
        call parse_error("No element name found - illegal name character")!FIXME ??
      endif
      allocate(fx%element_name(len_namebuffer(fb)))
      fx%element_name = vs_str(retrieve_namebuffer(fb))
    end subroutine parse_name

    subroutine parse_attributes
      character :: quotechar
      do 
        c = get_next_character_discarding_whitespace(fb, iostat)
        if (iostat/=0) return
        if (c=='/'.or.c=='>') then
          !FIXME and check for & which is illegal here I think
!          if (.not.c.in.fx%initialNameChars) then
 !           call parse_error("Illegal character in attribute name.")
 !           return
 !         endif
        endif
      call get_characters_until_not_namechar(fb, iostat)
      if (iostat/=0) return
      allocate(fx%element_name(len_namebuffer(fb)))
      fx%element_name = vs_str(retrieve_namebuffer(fb))
      c = get_next_character_discarding_whitespace(fb, iostat)
      if (iostat/=0) return
      if (c /= '=') then
        call parse_error("Expected = here.")
        return
      endif
      c = get_next_character_discarding_whitespace(fb, iostat)
      if (iostat/=0) return
      if (c =='"') then
        quotechar = '"'
      else if (c == "'") then
        quotechar = "'"
      else
        call parse_error("Expected ' or "" here.")
        return
      endif
      call get_characters_until_one_of(fb, quotechar, iostat)
      if (iostat/=0) return
      !FIXME can we ever get iostat=EOF through fill_buffer percolating too high?
!      call normalize_text(buffer_to_str(fx%buffer))
!        else
          call put_characters(fb, '!')!FIXME
          fx%state = WHITESPACE_IN_TAG
        !endif
      enddo
    end subroutine parse_attributes


    subroutine parse_entityreference
      character :: c_i
      ! Check first character of name to give better error messages.
      c = get_characters(fb, 1, iostat)
      if (iostat/=0) return
      if (c.in.whitespace) then
        call parse_error("Bare ampersand found."); return
      elseif (c==';') then
        call parse_error("No name found for entity reference."); return
      elseif (c=='#') then
        ! This is a character entity reference.
        c = get_characters(fb, 1, iostat)
        if (iostat/=0) return
        if (c=='x') then
          !It's hexadecimal
          !get characters until not one of hex
 !       elseif (c.in.digits) then
 !         !It's decimal
 !         ! get characters until not one of digit
        else
          call parse_error("Unexpected character found in entity reference"); return
        endif
        if (c/=';') then
          call parse_error("Expecting ; at end of character entity reference"); return
        endif
        ! Leave character references to be decoded later.
      elseif (c.in.initialNameChars) then
        c_i = c
        ! This is a general entity reference
        call get_characters_until_not_one_of(fb, XML1_0_NAMECHARS, iostat)
        if (iostat/=0) return
        c = get_characters(fb, 1, iostat) ! cannot fail
        if (c/=';') then
          call parse_error("Expecting ; at end of general entity reference"); return
        endif
        allocate(GEref(len_namebuffer(1)+1))
        GEref = c//vs_str(retrieve_namebuffer(fb))
        
        if (existing_entity(internal_ents, GEref)) then
          continue
 !         entity_sublist = copy_entity_list-without(ents, str_vs(GEref))
          !Expand entity & put into new fb buffer
!          call evolve_fsm(fb, entity_sublist, signal)
          !              ! Create new entity list without this entity
          !              call reparse(entity_contents, entity_list, signal)
          !              if (signal == BROKEN) then
          !                call parse_errorsomething
          !              else
          !                return
          !              endif
          !            else
        else
          call parse_error("Cannot reference unparsed entity in content"); return
        endif
      elseif (existing_entity(external_ents, GEref)) then
        call parse_error("FoX does not handle external reference"); return
        !FIXME maybe this should be a warning?
      else
        call parse_error("Reference to unknown entity"); return
      endif
          
          
    end subroutine parse_entityreference
      
    subroutine parse_error(msg)
      character(len=*) :: msg
      fx%state = ERROR
      deallocate(fx%action)
      allocate(fx%action(len(msg)))
      fx%action = vs_str(msg)
    end subroutine parse_error

    subroutine parse_action(msg)
      character(len=*) :: msg
      deallocate(fx%action)
      allocate(fx%action(len(msg)))
      fx%action = vs_str(msg)
    end subroutine parse_action

    subroutine normalize_text(s)
      character(len=*) :: s
    end subroutine normalize_text


  end subroutine evolve_fsm

end module m_sax_fsm
