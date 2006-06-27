module m_sax_dtd

  !Handling the Document Type Definition
  !Only very limited: just internal entities;
  !no external entities
  !And certainly no validation.

  use m_common_array_str, only : str_vs, vs_str
  use m_common_error, only: FoX_error
  use m_sax_entities, only: entity_list, add_char_entity, init_entity_list, destroy_entity_list

  implicit none
  private

  character(len=*), parameter :: spaces = " "//achar(9)//achar(10)//achar(13)

  character(len=*), parameter :: lowerCase = "abcdefghijklmnopqrstuvwxyz"
  character(len=*), parameter :: upperCase = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=*), parameter :: digits = "0123456789"
  character(len=*), parameter :: NameChars = lowerCase//upperCase//digits//".-_:"

  character(len=*), parameter :: PubIdChars = NameChars//spaces//"'()+,/=?;!*#@$%"

  integer, parameter :: DTD_INIT                  = 0
  integer, parameter :: DTD_DOCTYPE               = 1
  integer, parameter :: DTD_DOCTYPE_FOUND         = 2
  integer, parameter :: DTD_DOCTYPE_PUBLIC        = 3
  integer, parameter :: DTD_DOCTYPE_SYSTEM        = 4
  integer, parameter :: DTD_INTERNAL              = 5
  integer, parameter :: DTD_DECLARATION           = 6
  integer, parameter :: DTD_IGNORE_DECLARATION    = 7
  integer, parameter :: DTD_ENTITY                = 8
  integer, parameter :: DTD_ENTITY_NAME           = 9
  integer, parameter :: DTD_ENTITY_DEF            = 10
  integer, parameter :: DTD_ENTITY_PUBLIC         = 11
  integer, parameter :: DTD_ENTITY_SYSTEM         = 12
  integer, parameter :: DTD_ENTITY_NDATA          = 13
  integer, parameter :: DTD_ENTITY_NDATA_VALUE    = 14
  integer, parameter :: DTD_DECLARATION_DONE      = 15
  integer, parameter :: DTD_DONE                  = 16

  type dtd_parser
    character(len=1), dimension(:), pointer :: dtd
    character(len=1), dimension(:), pointer :: token
    character(len=1), dimension(:), pointer :: docTypeName
    character(len=1), dimension(:), pointer :: PublicId
    character(len=1), dimension(:), pointer :: SystemId
    character(len=1), dimension(:), pointer :: entityName
    character(len=1), dimension(:), pointer :: entityContent
    character(len=1), dimension(:), pointer :: entityPublicId
    character(len=1), dimension(:), pointer :: entitySystemId
    character(len=1), dimension(:), pointer :: NdataValue
    type(entity_list) :: pe_list
    integer :: dtd_state
    integer :: curr_pos
    logical :: external_found
    logical :: parameter_entity
  end type dtd_parser

  public :: parse_dtd

  logical, save :: debug = .true.

contains

  subroutine init_dtd_parser(parse_state, dtd)
    type(dtd_parser), intent(out) :: parse_state
    character(len=*), intent(in) :: dtd

    nullify(parse_state%dtd)
    allocate(parse_state%dtd(len(dtd)))
    parse_state%dtd = vs_str(dtd)

    allocate(parse_state%token(0))

    nullify(parse_state%docTypeName)
    nullify(parse_state%PublicID)
    nullify(parse_state%SystemId)
    nullify(parse_state%entityName)
    nullify(parse_state%entityContent)
    nullify(parse_state%entityPublicID)
    nullify(parse_state%entitySystemId)
    nullify(parse_state%ndataValue)
    call init_entity_list(parse_state%pe_list)

    parse_state%dtd_state = DTD_INIT
    parse_state%curr_pos = 1
    parse_state%external_found = .false.
  end subroutine init_dtd_parser

  subroutine destroy_dtd_parser(parse_state)
    type(dtd_parser), intent(inout) :: parse_state

    deallocate(parse_state%dtd)
    if (associated(parse_state%token)) deallocate(parse_state%token)
    if (associated(parse_state%docTypeName)) deallocate(parse_state%docTypeName)
    if (associated(parse_state%PublicId)) deallocate(parse_state%PublicId)
    if (associated(parse_state%SystemId)) deallocate(parse_state%SystemId)
    if (associated(parse_state%entityName)) deallocate(parse_state%entityName)
    if (associated(parse_state%entityContent)) deallocate(parse_state%entityContent)
    if (associated(parse_state%entityPublicId)) deallocate(parse_state%entityPublicId)
    if (associated(parse_state%entitySystemId)) deallocate(parse_state%entitySystemId)
    if (associated(parse_state%NdataValue)) deallocate(parse_state%NdataValue)

    call destroy_entity_list(parse_state%pe_list)

  end subroutine destroy_dtd_parser


  subroutine parse_dtd(dtd)
    character(len=*), intent(in):: dtd

    integer :: c, i, cp

    type(dtd_parser) :: parse_state

    call init_dtd_parser(parse_state, dtd)

    do 
      call tokenize_dtd(parse_state)

      if (size(parse_state%token) == 0) then
        select case(parse_state%dtd_state)
        case (DTD_DOCTYPE_FOUND, &
              DTD_DOCTYPE, &
              DTD_DONE)
          if (debug) print*,'DTD parsed successfully'
          call destroy_dtd_parser(parse_state)
          return
        case default
          call FoX_error("Unfinished DTD")
        end select
      endif

      select case (parse_state%dtd_state)
      case (DTD_INIT)
        if (str_vs(parse_state%token) == "DOCTYPE") then
          parse_state%dtd_state = DTD_DOCTYPE
          if (debug) print*,'DOCTYPE found'
        else
          call FoX_error("No DOCTYPE found")
        endif
        
      case (DTD_DOCTYPE)
        allocate(parse_state%docTypeName(size(parse_state%token)))
        parse_state%docTypeName = parse_state%token
        if (verify(str_vs(parse_state%docTypeName), NameChars) /= 0) &
          call FoX_error("Invalid DOCTYPE Name")
        parse_state%dtd_state = DTD_DOCTYPE_FOUND
        if (debug) print*,'DOCTYPE Name found'
        
      case (DTD_DOCTYPE_FOUND)
        if (.not.parse_state%external_found) then
          if (str_vs(parse_state%token) == "PUBLIC") then
            parse_state%dtd_state = DTD_DOCTYPE_PUBLIC
            if (debug) print*,'PUBLIC keyword found'
            cycle
          elseif (str_vs(parse_state%token) == "SYSTEM") then
            parse_state%dtd_state = DTD_DOCTYPE_SYSTEM
            if (debug) print*,'SYSTEM keyword found'
            cycle
          endif
        endif
        if (str_vs(parse_state%token) == "[") then
          parse_state%dtd_state = DTD_INTERNAL
          if (debug) print*,'Internal subset found'
        else
          call FoX_error("Invalid DTD found")
        endif
        
      case (DTD_DOCTYPE_PUBLIC)
        if ((parse_state%token(1) == "'" .and. &
          parse_state%token(size(parse_state%token)) == "'") &
          .or. &
          (parse_state%token(1) == '"' .and. &
          parse_state%token(size(parse_state%token)) == '"')) then
          allocate(parse_state%PublicId(size(parse_state%token) - 2))
          parse_state%PublicId = parse_state%token(2:size(parse_state%token)-1)
          if (verify(str_vs(parse_state%PublicId), PubIdChars) /= 0) &
            call FoX_error("Invalid PUBLIC ID")
        else
          call FoX_error("Badly quoted PUBLIC ref")
        endif
        parse_state%dtd_state = DTD_DOCTYPE_SYSTEM
        if (debug) print*,'PUBLIC ID found'
        
      case (DTD_DOCTYPE_SYSTEM)
        if ((parse_state%token(1) == "'" .and. &
          parse_state%token(size(parse_state%token)) == "'") &
          .or. &
          (parse_state%token(1) == '"' .and. &
          parse_state%token(size(parse_state%token)) == '"')) then
          allocate(parse_state%SystemId(size(parse_state%token) - 2))
          parse_state%SystemId = parse_state%token(2:size(parse_state%token)-1)
        else
          call FoX_error("Badly quoted SYSTEM ref")
        endif
        parse_state%external_found = .true.
        parse_state%dtd_state = DTD_DOCTYPE_FOUND
        if (debug) print*,'SYSTEM ID found'
        
      case (DTD_INTERNAL)
        if (str_vs(parse_state%token) == "<!") then
          parse_state%dtd_state = DTD_DECLARATION
          if (debug) print*,'DTD Declaration found'
        elseif (str_vs(parse_state%token) == "]") then
          parse_state%dtd_state = DTD_DONE
          if (debug) print*,'DTD internal subset ended'
        elseif (all(parse_state%token(:2) == (/'<','?'/))) then
          !handle PI
          if (debug) print*,'PI found'
          continue
        elseif (all(parse_state%token(:4) == (/'<','!','-','-'/))) then
          !handle comment
          if (debug) print*,'Comment found'
          continue
        else
          call FoX_error("Broken internal DTD")
        endif
        
      case (DTD_DECLARATION)
        if (str_vs(parse_state%token) == "ENTITY") then
          parse_state%dtd_state = DTD_ENTITY
          if (debug) print*,'DTD ENTITY started'
        elseif (str_vs(parse_state%token) == "ELEMENT") then
          continue ! unhandled FIXME
          parse_state%dtd_state = DTD_IGNORE_DECLARATION
          if (debug) print*,'DTD ELEMENT started'
        elseif (str_vs(parse_state%token) == "ATTLIST") then
          continue ! unhandled FIXME
          parse_state%dtd_state = DTD_IGNORE_DECLARATION
          if (debug) print*,'DTD ATTLIST started'
        elseif (str_vs(parse_state%token) == "NOTATION") then
          continue ! unhandled FIXME
          parse_state%dtd_state = DTD_IGNORE_DECLARATION
          if (debug) print*,'DTD NOTATION started'
        else
          call FoX_error("Broken internal DTD declaration")
        endif
        
      case (DTD_ENTITY)
        if (str_vs(parse_state%token) == '%') then
          parse_state%parameter_entity = .true.
          parse_state%dtd_state = DTD_ENTITY_NAME
          if (debug) print*,'DTD Parameter ENTITY found'
        else
          parse_state%parameter_entity = .false.
          if (associated(parse_state%entityName)) deallocate(parse_state%entityName)
          allocate(parse_state%entityName(size(parse_state%token)))
          parse_state%entityName = parse_state%token
          if (verify(str_vs(parse_state%entityName), NameChars) /= 0) &
            call FoX_error("Invalid Entity Name")
          parse_state%dtd_state = DTD_ENTITY_DEF
          if (debug) print*,'DTD ENTITY Name found'
        endif
        
      case (DTD_ENTITY_NAME)
        if (associated(parse_state%entityName)) deallocate(parse_state%entityName)
        allocate(parse_state%entityName(size(parse_state%token)))
        parse_state%entityName = parse_state%token
        if (verify(str_vs(parse_state%entityName), NameChars) /= 0) &
          call FoX_error("Invalid Entity Name")
        parse_state%dtd_state = DTD_ENTITY_DEF
        if (debug) print*,'DTD ENTITY Name found'
        
      case (DTD_ENTITY_DEF)
        if (str_vs(parse_state%token) == "PUBLIC") then
          parse_state%dtd_state = DTD_ENTITY_PUBLIC
          if (debug) print*,'DTD ENTITY PUBLIC keyword found'
        elseif (str_vs(parse_state%token) == "SYSTEM") then
          parse_state%dtd_state = DTD_ENTITY_SYSTEM
          if (debug) print*,'DTD ENTITY SYSTEM keyword found'
        else
          if ((parse_state%token(1) == "'" .and. &
            parse_state%token(size(parse_state%token)) == "'") &
            .or. &
            (parse_state%token(1) == '"' .and. &
            parse_state%token(size(parse_state%token)) == '"')) then
            if (associated(parse_state%entityContent)) deallocate(parse_state%entityContent)
            allocate(parse_state%entityContent(size(parse_state%token) - 2))
            parse_state%entityContent = parse_state%token(2:size(parse_state%token)-1)
            if (debug) print*,'DTD ENTITY content found'
            if (parse_state%parameter_entity) then
              call add_char_entity(parse_state%pe_list, &
                str_vs(parse_state%entityName), &
                str_vs(parse_state%entityContent))
              deallocate(parse_state%entityName)
              deallocate(parse_state%entityContent)
            else
              continue !FIXME
            endif
          else
            call FoX_error("Badly quoted ENTITY content")
          endif
          parse_state%dtd_state = DTD_DECLARATION_DONE
        endif
        
      case (DTD_ENTITY_PUBLIC)
        if ((parse_state%token(1) == "'" .and. &
          parse_state%token(size(parse_state%token)) == "'") &
          .or. &
          (parse_state%token(1) == '"' .and. &
          parse_state%token(size(parse_state%token)) == '"')) then
          if (associated(parse_state%entityPublicId)) deallocate(parse_state%entityPublicId)
          allocate(parse_state%entityPublicId(size(parse_state%token) - 2))
          parse_state%entityPublicId = parse_state%token(2:size(parse_state%token)-1)
          if (verify(str_vs(parse_state%entityPublicId), PubIdChars) /= 0) &
            call FoX_error("Invalid ENTITY PUBLIC ID")
        else
          call FoX_error("Badly quoted ENTITY PUBLIC ref")
        endif
        parse_state%dtd_state = DTD_ENTITY_SYSTEM
        if (debug) print*,'DTD ENTITY PUBLIC ID found'
        
      case (DTD_ENTITY_SYSTEM)
        if ((parse_state%token(1) == "'" .and. &
          parse_state%token(size(parse_state%token)) == "'") &
          .or. &
          (parse_state%token(1) == '"' .and. &
          parse_state%token(size(parse_state%token)) == '"')) then
          if (associated(parse_state%entitySystemId)) deallocate(parse_state%entitySystemId)
          allocate(parse_state%entitySystemId(size(parse_state%token) - 2))
          parse_state%entitySystemId = parse_state%token(2:size(parse_state%token)-1)
        else
          call FoX_error("Badly quoted ENTITY SYSTEM ref")
        endif
        parse_state%dtd_state = DTD_ENTITY_NDATA
        if (debug) print*,'DTD ENTITY SYSTEM ID found'
        
      case (DTD_ENTITY_NDATA)
        if (str_vs(parse_state%token) == "NDATA") then
          parse_state%dtd_state = DTD_ENTITY_NDATA_VALUE
          if (debug) print*,'DTD ENTITY NDATA keyword found'
        elseif (str_vs(parse_state%token) == ">") then
          parse_state%dtd_state = DTD_INTERNAL
          if (debug) print*,'DTD DECLARATION finished'
        else
          call FoX_error("Garbage found after ENTITY SYSTEM ref")
        endif
        
      case (DTD_ENTITY_NDATA_VALUE)
        allocate(parse_state%NdataValue(size(parse_state%token)))
        parse_state%NdataValue = parse_state%token
        if (verify(str_vs(parse_state%NdataValue), NameChars) /= 0) &
          call FoX_error("Invalid NDATA value")
        parse_state%dtd_state = DTD_DECLARATION_DONE
        if (debug) print*,'DTD ENTITY NDATA value found'

      case (DTD_IGNORE_DECLARATION)
        if (str_vs(parse_state%token) == ">") then
          parse_state%dtd_state = DTD_INTERNAL
          if (debug) print*,'DTD DECLARATION finished'
        endif
        
      case (DTD_DECLARATION_DONE)
        if (str_vs(parse_state%token) == ">") then
          parse_state%dtd_state = DTD_INTERNAL
          if (debug) print*,'DTD DECLARATION finished'
        else
          call FoX_error("Garbage found in DTD declaration")
        endif
        
      case (DTD_DONE)
        call FoX_error("Garbage found at end of DTD")

      case default
        call FoX_error("DTD parsing internal error")
        
      end select

    enddo

  end subroutine parse_dtd


  subroutine tokenize_dtd(parse_state)
    type(dtd_parser), intent(inout) :: parse_state

    integer :: c, cp, cp1

    deallocate(parse_state%token)
    c = parse_state%curr_pos
    cp = verify(str_vs(parse_state%dtd(c:)), spaces)
    if (cp == 0) then
      !nothing left but spaces
      allocate(parse_state%token(0))
      return
      
    elseif (cp == 1) then
      ! no spaces here, only the following tokens allowed:
      if (parse_state%dtd(c) == '[')  then
        continue
      elseif (parse_state%dtd(c) == ']') then
        continue
      elseif (all(parse_state%dtd(c:c+1) == (/'<','!'/))) then
        continue
      elseif (all(parse_state%dtd(c:c+1) == (/'<','?'/))) then
        continue
      elseif (parse_state%dtd(c) == '>') then
        continue
      elseif (all(parse_state%dtd(c:c+6) == vs_str('DOCTYPE')))then
        continue
      elseif (all(parse_state%dtd(c:c+5) == vs_str('ENTITY')))then
        continue
      elseif (all(parse_state%dtd(c:c+6) == vs_str('NOTATION')))then
        continue
      elseif (all(parse_state%dtd(c:c+6) == vs_str('ELEMENT')))then
        continue
      elseif (all(parse_state%dtd(c:c+6) == vs_str('ATTLIST')))then
        continue
      else
        call FoX_error("Tokenizing failed")
      endif
    endif

    c = c + cp - 1
    if (parse_state%dtd(c) == '[') then
      allocate(parse_state%token(1))
      parse_state%token = '['
      parse_state%curr_pos = c + 1
      return
      
    elseif (parse_state%dtd(c) == ']') then
      allocate(parse_state%token(1))
      parse_state%token = ']'
      parse_state%curr_pos = c + 1
      return
      
    elseif (all(parse_state%dtd(c:c+3) == (/'<','!','-','-'/))) then
      !it's a comment ...
      cp1 = index(str_vs(parse_state%dtd(c+3:)), '--')
      cp = index(str_vs(parse_state%dtd(c+3:)), '-->')
      if (cp1 < cp) then
        call FoX_error("Invalid comment in DTD")
      else
        allocate(parse_state%token(cp+6))
        parse_state%token = parse_state%dtd(c:cp+5)
        parse_state%curr_pos = c + cp + 5
      endif
      return

    elseif (all(parse_state%dtd(c:c+1) == (/'<','!'/))) then
      allocate(parse_state%token(2))
      parse_state%token = (/'<','!'/)
      parse_state%curr_pos = c + 2
      return
      
    elseif (all(parse_state%dtd(c:c+1) == (/'<','?'/))) then
      !it's a PI ...
      cp = index(str_vs(parse_state%dtd(c:)), '?>')
      allocate(parse_state%token(cp+2))
      parse_state%token = parse_state%dtd(c:cp+1)
      parse_state%curr_pos = c + cp + 1
      return

    elseif (parse_state%dtd(c) == '>') then
      allocate(parse_state%token(1))
      parse_state%token = '>'
      parse_state%curr_pos = c + 1
      return

    elseif (parse_state%dtd(c) == '"') then
      cp = index(str_vs(parse_state%dtd(c+1:)), '"') 
      allocate(parse_state%token(cp+1))
      parse_state%token = parse_state%dtd(c:c+cp)
      parse_state%curr_pos = c + cp  + 1
      return

    elseif (parse_state%dtd(c) == "'") then
      cp = index(str_vs(parse_state%dtd(c+1:)), "'") 
      allocate(parse_state%token(cp+1))
      parse_state%token = parse_state%dtd(c:c+cp)
      parse_state%curr_pos = c + cp + 1
      return

    elseif (all(parse_state%dtd(c:c+6) == vs_str('DOCTYPE')))then
      allocate(parse_state%token(7))
      parse_state%token = parse_state%dtd(c:c+6)
      parse_state%curr_pos = c + 7
      return

    else
      cp = index(str_vs(parse_state%dtd(c:)), '>')
      cp1 = scan(str_vs(parse_state%dtd(c:)), spaces)
      if (cp1 == 0) cp1 = size(parse_state%dtd) - c + 1
      if (cp1 < cp) cp = cp1
      allocate(parse_state%token(cp-1))
      parse_state%token = parse_state%dtd(c:c+cp-2)
      parse_state%curr_pos = c + cp - 1
      return

    endif

  end subroutine tokenize_dtd

end module m_sax_dtd
