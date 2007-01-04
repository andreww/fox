module m_wxml_core

  use m_common_attrs, only: dictionary_t, len, get_key, get_value, has_key, &
    add_item_to_dict, init_dict, reset_dict, destroy_dict
  use m_common_array_str, only: vs_str, str_vs, devnull
  use m_common_buffer, only: buffer_t, len, add_to_buffer, reset_buffer, &
    dump_buffer
  use m_common_charset, only: XML1_0, XML1_1
  use m_common_elstack, only: elstack_t, len, get_top_elstack, pop_elstack, &
    is_empty, init_elstack, push_elstack, destroy_elstack
  use m_common_entities, only: entity_list, init_entity_list, &
    destroy_entity_list, add_internal_entity, add_external_entity
  use m_common_error, only: FoX_warning_base, FoX_error_base, FoX_fatal_base
  use m_common_io, only: get_unit
  use m_common_namecheck, only: checkEncName, checkName, checkPITarget, &
    checkCharacterEntityReference, checkPubId, &
    checkQName, prefixOfQName, localpartofQName
  use m_common_namespaces, only: namespaceDictionary, getnamespaceURI, &
  initnamespaceDictionary, destroynamespaceDictionary, addDefaultNS, &
  addPrefixedNS, isPrefixInForce, checkNamespacesWriting, checkEndNamespaces
  use m_common_notations, only: notation_list, init_notation_list, &
    destroy_notation_list, add_notation, notation_exists
  use m_wxml_escape, only: escape_string

  use pxf, only: pxfabort

  implicit none
  private

  integer, parameter :: indent_inc = 2
  ! TOHW should we let this be set?

  !Output State Machines
  ! status wrt root element:
  integer, parameter :: WXML_STATE_1_JUST_OPENED = 0 
  !File is just opened, nothing written to it yet. 
  integer, parameter :: WXML_STATE_1_BEFORE_ROOT = 1
  !File has been opened, something has been written, but no root element yet.
  integer, parameter :: WXML_STATE_1_DURING_ROOT = 2
  !The root element has been opened but not closed
  integer, parameter :: WXML_STATE_1_AFTER_ROOT = 3
  !The root element has been opened but not closed

  ! status wrt tags:
  integer, parameter :: WXML_STATE_2_OUTSIDE_TAG = 0
  !We are not within a tag.
  integer, parameter :: WXML_STATE_2_INSIDE_PI = 1
  !We are inside a Processing Instruction tag
  integer, parameter :: WXML_STATE_2_INSIDE_ELEMENT = 2
  !We are inside an element tag.
  integer, parameter :: WXML_STATE_2_IN_CHARDATA = 3
  !We are inside deliberately-constructed text. (this is only necessary for preserve_whitespace)

  ! status wrt DTD
  integer, parameter :: WXML_STATE_3_BEFORE_DTD = 0
  ! No DTD has been encountered yet.
  integer, parameter :: WXML_STATE_3_DURING_DTD = 1
  ! Halfway throught outputting a DTD
  integer, parameter :: WXML_STATE_3_INSIDE_INTSUBSET = 2
  !We are inside the internal subset definition
  integer, parameter :: WXML_STATE_3_AFTER_DTD = 3
  ! Finished outputting a DTD
  


  type xmlf_t
    private
    character, pointer        :: filename(:)
    integer                   :: xml_version
    logical                   :: standalone = .false.
    integer                   :: lun = -1
    type(buffer_t)            :: buffer
    type(elstack_t)           :: stack
    type(dictionary_t)        :: dict
    integer                   :: state_1 = -1
    integer                   :: state_2 = -1
    integer                   :: state_3 = -1
    logical                   :: preserve_whitespace
    integer                   :: indent = 0
    character, pointer        :: name(:)
    type(namespaceDictionary) :: nsDict
    type(entity_list)         :: entityList
    type(entity_list)         :: PEList
    type(notation_list)       :: nList
  end type xmlf_t

  public :: xmlf_t

  public :: xml_OpenFile
  public :: xml_NewElement
  public :: xml_EndElement
  public :: xml_Close
  public :: xml_AddXMLDeclaration
  public :: xml_AddXMLStylesheet
  public :: xml_AddXMLPI
  public :: xml_AddComment
  public :: xml_AddCharacters
  public :: xml_AddNewline
  public :: xml_AddEntityReference
  public :: xml_AddAttribute
  public :: xml_AddPseudoAttribute
  public :: xml_DeclareNamespace
  public :: xml_UnDeclareNamespace
  public :: xml_AddDOCTYPE
  public :: xml_AddParameterEntity
  public :: xml_AddInternalEntity
  public :: xml_AddExternalEntity
  public :: xml_AddNotation
  public :: xml_AddElementToDTD
  public :: xml_AddAttlistToDTD
  public :: xml_AddPEreferenceToDTD

  public :: xmlf_Name
  public :: xmlf_OpenTag
 
  interface xml_AddCharacters
    module procedure xml_AddCharacters_Ch
  end interface
  interface xml_AddAttribute
    module procedure xml_AddAttribute_Ch
  end interface
  interface xml_AddPseudoAttribute
    module procedure xml_AddPseudoAttribute_Ch
  end interface
 
  !overload error handlers to allow file info
  interface wxml_warning
    module procedure wxml_warning_xf, FoX_warning_base
  end interface
  interface wxml_error
    module procedure wxml_error_xf, FoX_error_base
  end interface
  interface wxml_fatal
    module procedure wxml_fatal_xf, FoX_fatal_base
  end interface

  ! Heuristic (approximate) target for justification of output
  ! only gets used for outputting attributes
  integer, parameter  :: COLUMNS = 80

  ! TOHW - This is the longest string that may be output without
  ! a newline. The buffer must not be larger than this, but its size 
  ! can be tuned for performance.
  !lowest value found so far is 4096, for NAG. We use 1024 just in case.
  integer, parameter  :: xml_recl = 1024

contains

  subroutine xml_OpenFile(filename, xf, preserve_whitespace, broken_indenting, channel, replace, addDecl)
    character(len=*), intent(in)  :: filename
    type(xmlf_t), intent(inout)   :: xf
    logical, intent(in), optional :: preserve_whitespace
    logical, intent(in), optional :: broken_indenting
    integer, intent(in), optional :: channel
    logical, intent(in), optional :: replace
    logical, intent(in), optional :: addDecl
    
    integer :: iostat
    logical :: repl, decl

    if (xf%lun /= -1) &
      call wxml_fatal("Trying to reopen an already-open XML file")
    
    if (present(replace)) then
      repl = replace
    else
      repl = .true.
    endif
    if (present(addDecl)) then
      decl = addDecl
    else
      decl = .true.
    endif
    
    allocate(xf%filename(len(filename)))
    xf%filename = vs_str(filename)
    allocate(xf%name(0))
    
    if (present(channel)) then
      xf%lun = channel
    else
      call get_unit(xf%lun,iostat)
      if (iostat /= 0) call wxml_fatal(xf, "cannot open file: "//filename)
    endif
    
    ! Use large I/O buffer in case the O.S./Compiler combination
    ! has hard-limits by default (i.e., NAGWare f95's 1024 byte limit)
    ! This is related to the maximum size of the buffer.
    ! TOHW - This is the longest string that may be output without
    ! a newline. The buffer must not be larger than this, but its size 
    ! can be tuned for performance.
    
    if(repl) then
      open(unit=xf%lun, file=filename, form="formatted", status="replace", &
        action="write", recl=xml_recl)
    else 
      open(unit=xf%lun, file=filename, form="formatted", status="new", &
        action="write", recl=xml_recl)
    endif

    call init_elstack(xf%stack)
    
    call init_dict(xf%dict)
    !NB it can make no difference which XML version we are using
    !until after we output the XML declaration. So we set it to
    !1.0 for the moment & reset below.
    xf%xml_version = XML1_0
    
    xf%state_1 = WXML_STATE_1_JUST_OPENED
    xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    xf%state_3 = WXML_STATE_3_BEFORE_DTD
    
    if (present(preserve_whitespace)) then
      xf%preserve_whitespace = preserve_whitespace
    elseif (present(broken_indenting)) then
      xf%preserve_whitespace = .not.broken_indenting
    else
      xf%preserve_whitespace = .false.
    endif
      
    xf%indent = 0
    
    if (decl) then
      call xml_AddXMLDeclaration(xf,encoding='UTF-8')
      ! which will reset the buffer itself
    else
      call reset_buffer(xf%buffer, xf%lun, xf%xml_version)
    endif
    
    call initNamespaceDictionary(xf%nsDict)
    call init_entity_list(xf%entityList)
    call init_entity_list(xf%PEList)
    call init_notation_list(xf%nList)
    
  end subroutine xml_OpenFile


  subroutine xml_AddXMLDeclaration(xf, version, encoding, standalone)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in), optional :: version
    character(len=*), intent(in), optional :: encoding
    logical, intent(in), optional :: standalone

    call check_xf(xf)
    
    if (xf%state_1 /= WXML_STATE_1_JUST_OPENED) &
      call wxml_error("Tried to put XML declaration in wrong place")
    
    call reset_buffer(xf%buffer, xf%lun, xf%xml_version)
    
    call xml_AddXMLPI(xf, "xml", xml=.true.)
    if (present(version)) then
      if (version =="1.0") then
        xf%xml_version = XML1_0
        call xml_AddPseudoAttribute(xf, "version", version)
      elseif (version=="1.1") then
        xf%xml_version = XML1_1
        call xml_AddPseudoAttribute(xf, "version", version)
      else
        call wxml_error("Invalid XML version.")
      endif
    else
      call xml_AddPseudoAttribute(xf, "version", "1.0")
      xf%xml_version = XML1_0
    endif
    if (present(encoding)) then
      if (.not.checkEncName(encoding)) &
        call wxml_error("Invalid encoding name: "//encoding)
      if (encoding /= 'UTF-8' .and. encoding /= 'utf-8') &
        call wxml_warning("Non-default encoding specified: "//encoding)
      call xml_AddPseudoAttribute(xf, "encoding", encoding)
    endif
    if (present(standalone)) then
      xf%standalone = standalone
      if (standalone) then
        call xml_AddPseudoAttribute(xf, "standalone", "yes")
      else
        call xml_AddPseudoAttribute(xf, "standalone", "no")
      endif
    endif
    call close_start_tag(xf)
    ! We have to close explicitly here to ensure nothing gets tied
    ! up in the XML declaration
    xf%state_1 = WXML_STATE_1_BEFORE_ROOT
  end subroutine xml_AddXMLDeclaration


  subroutine xml_AddDOCTYPE(xf, name, system, public)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: system, public
    
    call check_xf(xf)
    
    call close_start_tag(xf)
    
    if (xf%state_1 /= WXML_STATE_1_BEFORE_ROOT) &
      call wxml_error("Tried to put XML DOCTYPE in wrong place: "//name)

    if (xf%state_3 /= WXML_STATE_3_BEFORE_DTD) then
      call wxml_error("Tried to output more than one DOCTYPE declaration: "//name)
    else
      xf%state_3 = WXML_STATE_3_DURING_DTD
    endif

    if (.not.checkName(name, xf%xml_version)) &
         call wxml_error("Invalid Name in DTD "//name)
    
    call add_eol(xf)
    call add_to_buffer("<!DOCTYPE "//name, xf%buffer)

    deallocate(xf%name)
    allocate(xf%name(len(name)))
    xf%name = vs_str(name)

    if (present(system)) then
      if (present(public)) then
        if (.not.checkPubId(public)) &
          call wxml_error("Invalid PUBLIC ID "//public)
        if (scan(public, "'") /= 0) then
          call add_to_buffer(' PUBLIC "'//public//'" ', xf%buffer)
        else
          call add_to_buffer(" PUBLIC '"//public//"' ", xf%buffer)
        endif
      else
        call add_to_buffer(' SYSTEM ', xf%buffer)
      endif
      if (scan(system, "'")/=0) then
        if (scan(system, '"')/=0) &
          call wxml_error("Invalid SYSTEM ID "//system)
        call add_to_buffer('"'//system//'"', xf%buffer)
      else
        call add_to_buffer("'"//system//"'", xf%buffer)
      endif
    elseif (present(public)) then
      call wxml_error("wxml:DOCTYPE: PUBLIC supplied without SYSTEM for: "//name)
    endif
    
  end subroutine xml_AddDOCTYPE


  subroutine xml_AddParameterEntity(xf, name, PEdef, system, public)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: PEDef
    character(len=*), intent(in), optional :: system
    character(len=*), intent(in), optional :: public
    
    call check_xf(xf)
    
    if (xf%state_3 == WXML_STATE_3_DURING_DTD) then
      call add_to_buffer(" [", xf%buffer)
      xf%state_3 = WXML_STATE_3_INSIDE_INTSUBSET
    endif

    if (xf%state_3 /= WXML_STATE_3_INSIDE_INTSUBSET) &
      call wxml_fatal("Cannot define Parameter Entity here: "//name)
      
    if (xf%state_2 == WXML_STATE_2_INSIDE_PI) then
      call close_start_tag(xf)
      xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    endif

    if (present(PEdef)) then
      if (present(system) .or. present(public)) &
        call wxml_fatal("Parameter entity "//name//" cannot have both a PEdef and an External ID")
    else
      if (.not.present(system)) &
        call wxml_fatal("Parameter entity "//name//" must have either a PEdef or an External ID")
    endif
    if (present(PEdef)) then
      call add_internal_entity(xf%PEList, name, PEdef, xf%xml_version)
    else
      call add_external_entity(xf%PEList, name, xf%xml_version, system, public)
    endif

    call add_eol(xf)

    call add_to_buffer('<!ENTITY % '//name//' ', xf%buffer)
    if (present(PEdef)) then
      if (index(PEdef, '"') > 0) then
        call add_to_buffer("'"//PEdef//"'>", xf%buffer)
      else
        call add_to_buffer('"'//PEdef//'">', xf%buffer)
      endif
    else
      if (present(public)) then
        if (index(public, '"') > 0) then
          call add_to_buffer("PUBLIC '"//public//"' ", xf%buffer)
        else
          call add_to_buffer('PUBLIC "'//public//'" ', xf%buffer)
        endif
      else
        call add_to_buffer('SYSTEM ', xf%buffer)
      endif
      if (index(system, '"') > 0) then
        call add_to_buffer("'"//system//'"', xf%buffer)
      else
        call add_to_buffer("'"//system//"'", xf%buffer)
      endif
      call add_to_buffer(">", xf%buffer)
    endif
  end subroutine xml_AddParameterEntity


  subroutine xml_AddInternalEntity(xf, name, value)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: value

    call check_xf(xf)
    
    if (xf%state_3 == WXML_STATE_3_DURING_DTD) then
      call add_to_buffer(" [", xf%buffer)
      xf%state_3 = WXML_STATE_3_INSIDE_INTSUBSET
    endif

    if (xf%state_3 /= WXML_STATE_3_INSIDE_INTSUBSET) &
      call wxml_fatal("Cannot define Entity here: "//name)
      
    if (xf%state_2 == WXML_STATE_2_INSIDE_PI) then
      call close_start_tag(xf)
      xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    endif

    call add_internal_entity(xf%entityList, name, value, xf%xml_version)

    call add_eol(xf)
    
    call add_to_buffer('<!ENTITY '//name//' ', xf%buffer)
    if (index(value, '"') > 0) then
      call add_to_buffer("'"//value//"'>", xf%buffer)
    else
      call add_to_buffer('"'//value//'">', xf%buffer)
    endif

  end subroutine xml_AddInternalEntity


  subroutine xml_AddExternalEntity(xf, name, system, public, notation)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: system
    character(len=*), intent(in), optional :: public
    character(len=*), intent(in), optional :: notation

    call check_xf(xf)
    
    if (xf%state_3 == WXML_STATE_3_DURING_DTD) then
      call add_to_buffer(" [", xf%buffer)
      xf%state_3 = WXML_STATE_3_INSIDE_INTSUBSET
    endif

    if (xf%state_3 /= WXML_STATE_3_INSIDE_INTSUBSET) &
      call wxml_fatal("Cannot define Entity here: "//name)

    if (xf%state_2 == WXML_STATE_2_INSIDE_PI) then
      call close_start_tag(xf)
      xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    endif

    !Ideally we'd check here - but perhaps the notation has been specified
    ! externally ...

    if (present(notation)) then
      if (.not.notation_exists(xf%nList, notation)) &
        call wxml_warning("Tried to add possibly unregistered notation to entity: "//name)
    endif
      
    !Name checking is done within add_external_entity
    call add_external_entity(xf%entityList, name, xf%xml_version, system, public, notation)
    
    call add_eol(xf)
    
    call add_to_buffer('<!ENTITY '//name, xf%buffer)
    if (present(public)) then
      if (index(public, '"') > 0) then
        call add_to_buffer(" PUBLIC '"//public//"' ", xf%buffer)
      else
        call add_to_buffer(' PUBLIC "'//public//'" ', xf%buffer)
      endif
    else
      call add_to_buffer(' SYSTEM ', xf%buffer)
    endif
    if (index(system, '"') > 0) then
      call add_to_buffer("'"//system//'"', xf%buffer)
    else
      call add_to_buffer("'"//system//"'", xf%buffer)
    endif
    if (present(notation)) then
      call add_to_buffer(' NDATA '//notation, xf%buffer)
    endif
    call add_to_buffer('>', xf%buffer)
      
  end subroutine xml_AddExternalEntity


  subroutine xml_AddNotation(xf, name, system, public)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    character(len=*), intent(in), optional :: system
    character(len=*), intent(in), optional :: public

    call check_xf(xf)
    
    if (xf%state_3 == WXML_STATE_3_DURING_DTD) then
      call add_to_buffer(" [", xf%buffer)
      xf%state_3 = WXML_STATE_3_INSIDE_INTSUBSET
    endif

    if (xf%state_3 /= WXML_STATE_3_INSIDE_INTSUBSET) &
      call wxml_fatal("Cannot define Notation here: "//name)
    
    if (xf%state_2 == WXML_STATE_2_INSIDE_PI) then
      call close_start_tag(xf)
      xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    endif

    if (notation_exists(xf%nList, name)) &
      call wxml_fatal("Tried to create duplicate notation: "//name)
    
    call add_eol(xf)

    call add_notation(xf%nList, name, xf%xml_version, system, public)
    call add_to_buffer('<!NOTATION '//name, xf%buffer)
    if (present(public)) then
      if (index(public, '"') > 0) then
        call add_to_buffer(" PUBLIC '"//public//"' ", xf%buffer)
      else
        call add_to_buffer(' PUBLIC "'//public//'" ', xf%buffer)
      endif
    elseif (present(system)) then
      call add_to_buffer(' SYSTEM ', xf%buffer)
    endif
    if (present(system)) then
      if (index(system, '"') > 0) then
        call add_to_buffer("'"//system//'"', xf%buffer)
      else
        call add_to_buffer("'"//system//"'", xf%buffer)
      endif
    endif
    call add_to_buffer('>', xf%buffer)
    
  end subroutine xml_AddNotation


  subroutine xml_AddElementToDTD(xf, name, declaration)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: declaration

    call check_xf(xf)

    if (.not.checkName(name, xf%xml_version)) &
      call wxml_error("Element name is illegal in xml_AddElementToDTD: "//name)

    !FIXME we should check declaration syntax too.
    call wxml_warning("Adding ELEMENT declaration to DTD. Cannot guarantee well-formedness")
    
    if (xf%state_3 == WXML_STATE_3_DURING_DTD) then
      call add_to_buffer(" [", xf%buffer)
      xf%state_3 = WXML_STATE_3_INSIDE_INTSUBSET
    endif

    if (xf%state_3 /= WXML_STATE_3_INSIDE_INTSUBSET) &
      call wxml_fatal("Cannot write to DTD here: xml_AddElementToDTD")

    if (xf%state_2 == WXML_STATE_2_INSIDE_PI) then
      call close_start_tag(xf)
      xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    endif

    call add_eol(xf)
    call add_to_buffer('<!ELEMENT '//name//' '//declaration//'>', xf%buffer)

  end subroutine xml_AddElementToDTD


  subroutine xml_AddAttlistToDTD(xf, name, declaration)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: declaration

    call check_xf(xf)

    if (.not.checkName(name, xf%xml_version)) &
      call wxml_error("Attlist name is illegal in xml_AddAttlistToDTD: "//name)

    !FIXME we should check declaration syntax too.
    call wxml_warning("Adding ATTLIST declaration to DTD. Cannot guarantee well-formedness")
    
    if (xf%state_3 == WXML_STATE_3_DURING_DTD) then
      call add_to_buffer(" [", xf%buffer)
      xf%state_3 = WXML_STATE_3_INSIDE_INTSUBSET
    endif

    if (xf%state_3 /= WXML_STATE_3_INSIDE_INTSUBSET) &
      call wxml_fatal("Cannot write to DTD here: xml_AddAttlistToDTD")

    if (xf%state_2 == WXML_STATE_2_INSIDE_PI) then
      call close_start_tag(xf)
      xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    endif

    call add_eol(xf)
    call add_to_buffer('<!ATTLIST '//name//' '//declaration//'>', xf%buffer)

  end subroutine xml_AddAttlistToDTD
    

  subroutine xml_AddPEReferenceToDTD(xf, name)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name

    call check_xf(xf)

    if (.not.checkName(name, xf%xml_version)) &
      call wxml_error("Trying to add illegal name in xml_AddPEReferenceToDTD: "//name)

    call wxml_warning("Adding PEReference to DTD. Cannot guarantee well-formedness")
    
    if (xf%state_3 == WXML_STATE_3_DURING_DTD) then
      call add_to_buffer(" [", xf%buffer)
      xf%state_3 = WXML_STATE_3_INSIDE_INTSUBSET
    endif

    if (xf%state_3 /= WXML_STATE_3_INSIDE_INTSUBSET) &
      call wxml_fatal("Cannot write to DTD here: xml_AddPEReferenceToDTD")

    if (xf%state_2 == WXML_STATE_2_INSIDE_PI) then
      call close_start_tag(xf)
      xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    endif

    call add_eol(xf)
    call add_to_buffer('%'//name//';', xf%buffer)

  end subroutine xml_AddPEReferenceToDTD


  subroutine xml_AddXMLStylesheet(xf, href, type, title, media, charset, alternate)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in) :: href
    character(len=*), intent(in) :: type
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: media
    character(len=*), intent(in), optional :: charset
    logical,          intent(in), optional :: alternate
    
    call check_xf(xf)
    
    if (xf%state_1 /= WXML_STATE_1_JUST_OPENED &
         .and. xf%state_1 /= WXML_STATE_1_BEFORE_ROOT) &
      call wxml_error("Cannot add stylesheet here: "//href)

    call close_start_tag(xf)
    
    call xml_AddXMLPI(xf, 'xml-stylesheet', xml=.true.)
    call xml_AddPseudoAttribute(xf, 'href', href)
    call xml_AddPseudoAttribute(xf, 'type', type)
    
    if (present(title)) call xml_AddPseudoAttribute(xf, 'title', title)
    if (present(media)) call xml_AddPseudoAttribute(xf, 'media', media)
    if (present(charset)) call xml_AddPseudoAttribute(xf, 'charset', charset)
    if (present(alternate)) then
      if (alternate) then
        call xml_AddPseudoAttribute(xf, 'alternate', 'yes')
      else
        call xml_AddPseudoAttribute(xf, 'alternate', 'no')
      endif
    endif
    if (xf%state_1 == WXML_STATE_1_JUST_OPENED) &
         xf%state_1 = WXML_STATE_1_BEFORE_ROOT 
    xf%state_2 = WXML_STATE_2_INSIDE_PI
    
  end subroutine xml_AddXMLStylesheet
  

  subroutine xml_AddXMLPI(xf, name, data, xml)
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in)           :: name
    character(len=*), intent(in), optional :: data
    logical, optional :: xml

    call check_xf(xf)
    
    select case (xf%state_1)
    case (WXML_STATE_1_JUST_OPENED) 
      xf%state_1 = WXML_STATE_1_BEFORE_ROOT
    case (WXML_STATE_1_BEFORE_ROOT)
      call close_start_tag(xf)
      call add_eol(xf)
    case (WXML_STATE_1_DURING_ROOT)
      call close_start_tag(xf)
    case (WXML_STATE_1_AFTER_ROOT)
      call close_start_tag(xf)
      call add_eol(xf)
    end select

    if (.not.present(xml) .and. .not.checkPITarget(name, xf%xml_version)) &
         call wxml_error(xf, "Invalid PI Target "//name)
    call add_to_buffer("<?" // name, xf%buffer)
    if (present(data)) then
      if (index(data, '?>') > 0) &
           call wxml_error(xf, "Tried to output invalid PI data "//data)
      call add_to_buffer(' '//data//'?>', xf%buffer)
      ! state_2 is now OUTSIDE_TAG from close_start_tag
    else
      xf%state_2 = WXML_STATE_2_INSIDE_PI
      call reset_dict(xf%dict)
    endif

  end subroutine xml_AddXMLPI


  subroutine xml_AddComment(xf,comment)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: comment
    
    call check_xf(xf)
    
    select case (xf%state_1)
    case (WXML_STATE_1_JUST_OPENED) 
      xf%state_1 = WXML_STATE_1_BEFORE_ROOT
    case (WXML_STATE_1_BEFORE_ROOT)
      call close_start_tag(xf)
      call add_eol(xf)
    case (WXML_STATE_1_DURING_ROOT)
      call close_start_tag(xf)
      if (.not.xf%preserve_whitespace.and.xf%state_2 == WXML_STATE_2_OUTSIDE_TAG) call add_eol(xf)
    case (WXML_STATE_1_AFTER_ROOT)
      call close_start_tag(xf)
      call add_eol(xf)
    end select

    if (index(comment,'--') > 0 .or. comment(len(comment):) == '-') &
         call wxml_error("Tried to output invalid comment "//comment)

    call add_to_buffer("<!--", xf%buffer)
    call add_to_buffer(comment, xf%buffer)
    call add_to_buffer("-->", xf%buffer)

  end subroutine xml_AddComment


  subroutine xml_NewElement(xf, name)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name

    call check_xf(xf)
    
    select case (xf%state_1)
    case (WXML_STATE_1_JUST_OPENED, WXML_STATE_1_BEFORE_ROOT)
      if (size(xf%name) > 0) then
        if (str_vs(xf%name) /= name) & 
          call wxml_error(xf, "Root element name does not match DTD: "//name)
      endif
      call close_start_tag(xf)
      if (xf%state_3 /= WXML_STATE_3_BEFORE_DTD) then
        select case (xf%state_3)
        case (WXML_STATE_3_DURING_DTD)
          call add_to_buffer('>', xf%buffer)
          xf%state_3 = WXML_STATE_3_AFTER_DTD
        case (WXML_STATE_3_INSIDE_INTSUBSET)
          xf%state_3 = WXML_STATE_3_AFTER_DTD
          call add_eol(xf)
          call add_to_buffer(']>', xf%buffer)
        end select
      endif
      call add_eol(xf)
    case (WXML_STATE_1_DURING_ROOT)
      call close_start_tag(xf)
      if (.not.xf%preserve_whitespace.and.xf%state_2 == WXML_STATE_2_OUTSIDE_TAG) call add_eol(xf)
    case (WXML_STATE_1_AFTER_ROOT)
      call wxml_error(xf, "Two root elements: "//name)
    end select
    
    if (.not.checkQName(name, xf%xml_version)) then
      call wxml_error(xf, 'Element name '//name//' is not valid')
    endif

    if (len(prefixOfQName(name)) > 0) then
      if (.not.isPrefixInForce(xf%nsDict, prefixOfQName(name))) &
        call wxml_error(xf, "Namespace prefix not registered: "//prefixOfQName(name))
    endif
    
    call push_elstack(name,xf%stack)
    call add_to_buffer("<"//name, xf%buffer)
    xf%state_2 = WXML_STATE_2_INSIDE_ELEMENT
    call reset_dict(xf%dict)
    xf%indent = xf%indent + indent_inc
    xf%state_1 = WXML_STATE_1_DURING_ROOT

  end subroutine xml_NewElement
  

  subroutine xml_AddCharacters_ch(xf, chars, parsed)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: chars
    logical, intent(in), optional :: parsed

    logical :: pc

    call check_xf(xf)
    
    if (xf%state_1 /= WXML_STATE_1_DURING_ROOT) &
         call wxml_fatal("Tried to add text section in wrong place: "//chars)
    
    if (present(parsed)) then
      pc = parsed
    else
      pc = .true.
    endif
    
    call close_start_tag(xf)

    if (pc) then
      call add_to_buffer(escape_string(chars, xf%xml_version), xf%buffer)
    else
      if (index(chars,']]>') > 0) &
           call wxml_fatal("Tried to output invalid CDATA: "//chars)
      call add_to_buffer("<![CDATA["//chars//"]]>", xf%buffer)
    endif
    
    xf%state_2 = WXML_STATE_2_IN_CHARDATA
  end subroutine xml_AddCharacters_Ch


  subroutine xml_AddNewline(xf)
    type(xmlf_t), intent(inout) :: xf
    
    call xml_AddCharacters(xf, "")
    call add_eol(xf)
  end subroutine xml_AddNewline

  
  subroutine xml_AddEntityReference(xf, entityref)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: entityref

    call check_xf(xf)
    
    !Where can we add this? If we allow the full gamut
    !of entities, we can no longer properly ensure
    !well-formed output, unless we tie the sax parser
    !in as well ...

    call close_start_tag(xf)

    if (xf%state_2 /= WXML_STATE_2_OUTSIDE_TAG .and. &
      xf%state_2 /= WXML_STATE_2_IN_CHARDATA)         &
      call wxml_fatal("Tried to add entity reference in wrong place: "//entityref)

    if (.not.checkCharacterEntityReference(entityref, xf%xml_version)) then
      !it's not just a unicode entity
      call wxml_warning("Entity reference added - document may not be well-formed")
    endif
    call add_to_buffer('&'//entityref//';', xf%buffer)
    xf%state_2 = WXML_STATE_2_IN_CHARDATA
  end subroutine xml_AddEntityReference


  subroutine xml_AddAttribute_Ch(xf, name, value, escape)
    type(xmlf_t), intent(inout)             :: xf
    character(len=*), intent(in)            :: name
    character(len=*), intent(in)            :: value
    logical, intent(in), optional           :: escape

    logical :: esc

    call check_xf(xf)
    
    if (present(escape)) then
      esc = escape
    else
      esc = .true.
    endif

    !FIXME when escape is false we should still verify somehow.
    !minimal check: only extra allowed is a character entity reference

    if (.not.esc) &
      call wxml_warning("Outputting unescaped attribute value. Cannot guarantee well-formedness")

    if (xf%state_2 /= WXML_STATE_2_INSIDE_ELEMENT) &
         call wxml_error(xf, "attributes outside element content: "//name)
    
    if (has_key(xf%dict,name)) &
         call wxml_error(xf, "duplicate att name: "//name)
    
    if (.not.checkQName(name, xf%xml_version)) &
         call wxml_error(xf, "invalid attribute name: "//name)

    if (len(prefixOfQName(name))>0) then
      if (.not.isPrefixInForce(xf%nsDict, prefixOfQName(name))) &
        call wxml_error(xf, "namespace prefix not registered: "//prefixOfQName(name))
      if (esc) then
        call add_item_to_dict(xf%dict, localpartofQname(name), escape_string(value, xf%xml_version), prefixOfQName(name), &
          getnamespaceURI(xf%nsDict,prefixOfQname(name)))
      else
        call add_item_to_dict(xf%dict, localpartofQname(name), value, prefixOfQName(name), &
          getnamespaceURI(xf%nsDict,prefixOfQName(name)))
      endif
    else
      if (esc) then
        call add_item_to_dict(xf%dict, name, escape_string(value, xf%xml_version))
      else
        call add_item_to_dict(xf%dict, name, value)
      endif
    endif
    
  end subroutine xml_AddAttribute_Ch


  subroutine xml_AddPseudoAttribute_Ch(xf, name, value, escape)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name
    character(len=*), intent(in)  :: value
    logical, intent(in), optional           :: escape

    logical :: esc

    call check_xf(xf)
    
    if (present(escape)) then
      esc = escape
    else
      esc = .true.
    endif

    if (xf%state_2 /= WXML_STATE_2_INSIDE_PI) &
         call wxml_error("PI pseudo-attribute outside PI: "//name)

    ! This is mostly ad-hoc, pseudo-attribute names are not defined anywhere.
    if (.not.checkName(name, xf%xml_version)) &
         call wxml_error("Invalid pseudo-attribute name: "//name)

    if (has_key(xf%dict,name)) &
         call wxml_error(xf, "duplicate pseudo-attribute name: "//name)

    if (index(value, '?>') > 0) &
         call wxml_error(xf, "Invalid pseudo-attribute data: "//value)
    
    if (esc) then
      call add_item_to_dict(xf%dict, name, escape_string(value, xf%xml_version))
    else
      call add_item_to_dict(xf%dict, name, value)
    endif
    
  end subroutine xml_AddPseudoAttribute_Ch


  subroutine xml_EndElement(xf, name)
    type(xmlf_t), intent(inout)             :: xf
    character(len=*), intent(in)            :: name

    call check_xf(xf)

    if (len(xf%stack) == 0) &
      call wxml_fatal(xf,'Trying to close '//name//' but no tags are open.')

    if (get_top_elstack(xf%stack) /= name) &
      call wxml_fatal(xf, 'Trying to close '//name//' but '//get_top_elstack(xf%stack)// &
      ' is open. Either you have failed to open '//get_top_elstack(xf%stack)//&
      ' or you have failed to close '//name//'.') 
    xf%indent = xf%indent - indent_inc
 
    select case (xf%state_2)
    case (WXML_STATE_2_INSIDE_ELEMENT)
      call checkNamespacesWriting(xf%dict, xf%nsDict, len(xf%stack))
      if (len(xf%dict) > 0) call write_attributes(xf)
      if (xf%preserve_whitespace) call add_eol(xf)
      call add_to_buffer("/>",xf%buffer)
      call devnull(pop_elstack(xf%stack))
    case (WXML_STATE_2_OUTSIDE_TAG, WXML_STATE_2_IN_CHARDATA)
      if (.not.xf%preserve_whitespace.and.xf%state_2==WXML_STATE_2_OUTSIDE_TAG) call add_eol(xf)
! XLF does a weird thing here, and if pop_elstack is called as an 
! argument to the call, it gets called twice. So we have to separate
! out get_top_... from pop_...
      call add_to_buffer("</" //get_top_elstack(xf%stack), xf%buffer)
      call devnull(pop_elstack(xf%stack))
      if (xf%preserve_whitespace) call add_eol(xf)
      call add_to_buffer(">", xf%buffer)
    case (WXML_STATE_2_INSIDE_PI)
      call close_start_tag(xf)
    end select

    call checkEndNamespaces(xf%nsDict, len(xf%stack)+1)
    
    if (is_empty(xf%stack)) then
      xf%state_1 = WXML_STATE_1_AFTER_ROOT
    endif
    xf%state_2 = WXML_STATE_2_OUTSIDE_TAG

  end subroutine xml_EndElement


  subroutine xml_DeclareNamespace(xf, nsURI, prefix, xml)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in) :: nsURI
    character(len=*), intent(in), optional :: prefix
    logical, intent(in), optional :: xml

    call check_xf(xf)

    if (xf%state_1 == WXML_STATE_1_AFTER_ROOT) &
      call wxml_error(xf, "adding namespace outside element content")

    if (len(nsURI) == 0) &
      call wxml_error(xf, "adding namespace with empty URI")
    
    if (present(prefix)) then
      call addPrefixedNS(xf%nsDict, prefix, nsURI, len(xf%stack)+1, xf%xml_version, xml)
    else
      call addDefaultNS(xf%nsDict, nsURI, len(xf%stack)+1)
    endif
    
  end subroutine xml_DeclareNamespace


  subroutine xml_UndeclareNamespace(xf, prefix)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in), optional :: prefix
    
    call check_xf(xf)

    if (xf%xml_version == XML1_0) &
      call wxml_error("cannot undeclare namespaces in XML 1.0")
    
    if (xf%state_1 == WXML_STATE_1_AFTER_ROOT) &
      call wxml_error(xf, "Undeclaring namespace outside element content")
    
    if (present(prefix)) then
      call addPrefixedNS(xf%nsDict, prefix, "", len(xf%stack)+1, XML1_1)
    else
      call addDefaultNS(xf%nsDict, "", len(xf%stack)+1)
    endif
    
  end subroutine xml_UndeclareNamespace


  subroutine xml_Close(xf)
    type(xmlf_t), intent(inout)   :: xf

    if (xf%lun == -1) &
      call wxml_fatal('Tried to close XML file which is not open')

    if (xf%state_2 == WXML_STATE_2_INSIDE_PI) &
      call close_start_tag(xf)

    if (xf%state_3 /= WXML_STATE_3_BEFORE_DTD &
      .and. xf%state_3 /= WXML_STATE_3_AFTER_DTD) then
      select case (xf%state_3)
      case (WXML_STATE_3_DURING_DTD)
        call add_to_buffer('>', xf%buffer)
      case (WXML_STATE_3_INSIDE_INTSUBSET)
        call add_eol(xf)
        call add_to_buffer(']>', xf%buffer)
      end select
      xf%state_3 = WXML_STATE_3_AFTER_DTD
    endif
    
    do while (xf%state_1 == WXML_STATE_1_DURING_ROOT)
      if (xf%state_1 == WXML_STATE_1_AFTER_ROOT) exit
      call xml_EndElement(xf, get_top_elstack(xf%stack))
    enddo

    if (xf%state_1 /= WXML_STATE_1_AFTER_ROOT) &
      call wxml_warning(xf, 'Invalid XML document produced: No root element')
    
    call dump_buffer(xf%buffer)
    close(unit=xf%lun)
    xf%lun = -1

    call destroy_dict(xf%dict)
    call destroy_elstack(xf%stack)
    
    call destroyNamespaceDictionary(xf%nsDict)
    call destroy_entity_list(xf%entityList)
    call destroy_entity_list(xf%PEList)
    call destroy_notation_list(xf%nList)
    
    deallocate(xf%name)
    deallocate(xf%filename)
    
  end subroutine xml_Close


!==================================================================
  !----------------------------------------------------------

  subroutine check_xf(xf)
    type(xmlf_t), intent(inout)   :: xf
    if (xf%lun == -1) &
      call wxml_fatal("Tried to manipulate an XML File which is not open")

  end subroutine check_xf


  subroutine add_eol(xf)
    type(xmlf_t), intent(inout)   :: xf
    
    integer :: indent_level
    
    ! In case we still have a zero-length stack, we must make
    ! sure indent_level is not less than zero.
    if (xf%state_3 == WXML_STATE_3_INSIDE_INTSUBSET) then
      indent_level = indent_inc
    else
      indent_level = xf%indent
    endif
    
    !We must flush here (rather than just adding an eol character)
    !since we don't know what the eol character is on this system.
    !Flushing with a linefeed will get it automatically, though.
    call dump_buffer(xf%buffer, lf=.true.)
    call reset_buffer(xf%buffer, xf%lun, xf%xml_version)
    
    if (.not.xf%preserve_whitespace) &
      call add_to_buffer(repeat(' ',indent_level),xf%buffer)
    
  end subroutine add_eol
  

  subroutine close_start_tag(xf)
    type(xmlf_t), intent(inout)   :: xf
    
    select case (xf%state_2)
    case (WXML_STATE_2_INSIDE_ELEMENT)
      call checkNamespacesWriting(xf%dict, xf%nsDict, len(xf%stack))
      if (len(xf%dict) > 0)  call write_attributes(xf)
      if (.not.xf%preserve_whitespace) then
        call add_to_buffer('>', xf%buffer)
      else
        call add_eol(xf)
        call add_to_buffer('>', xf%buffer)
      endif
      xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    case (WXML_STATE_2_INSIDE_PI)
      if (len(xf%dict) > 0)  call write_attributes(xf)
      call add_to_buffer('?>', xf%buffer)
      xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
    case (WXML_STATE_2_IN_CHARDATA)
      continue
    case (WXML_STATE_2_OUTSIDE_TAG)
      continue
    end select
    
  end subroutine close_start_tag


  subroutine write_attributes(xf)
    type(xmlf_t), intent(inout)   :: xf

    integer  :: i, size
    
    if (xf%state_2 /= WXML_STATE_2_INSIDE_PI .and. &
      xf%state_2 /= WXML_STATE_2_INSIDE_ELEMENT) &
      call wxml_fatal("Internal library error")
    
    do i = 1, len(xf%dict)
      size = len(get_key(xf%dict, i)) + len(get_value(xf%dict, i)) + 4
      if ((len(xf%buffer) + size) > COLUMNS) then
        call add_eol(xf)
      else
        call add_to_buffer(" ", xf%buffer)
      endif
      call add_to_buffer(get_key(xf%dict, i), xf%buffer)
      call add_to_buffer("=", xf%buffer)
      call add_to_buffer("""",xf%buffer)
      call add_to_buffer(get_value(xf%dict, i), xf%buffer)
      call add_to_buffer("""", xf%buffer)
    enddo
    
    
  end subroutine write_attributes
  
!---------------------------------------------------------
! Error handling/trapping routines:

    subroutine wxml_warning_xf(xf, msg)
      ! Emit warning, but carry on.
      type(xmlf_t), intent(in) :: xf
      character(len=*), intent(in) :: msg

      write(6,'(a)') 'WARNING(wxml) in writing to file ', xmlf_name(xf)
      write(6,'(a)')  msg

    end subroutine wxml_warning_xf

    subroutine wxml_error_xf(xf, msg)
      ! Emit error message, clean up file and stop.
      type(xmlf_t), intent(inout) :: xf
      character(len=*), intent(in) :: msg

      write(6,'(a)') 'ERROR(wxml) in writing to file ', xmlf_name(xf)
      write(6,'(a)')  msg

      call xml_Close(xf)
      stop

    end subroutine wxml_error_xf


    subroutine wxml_fatal_xf(xf, msg)
      !Emit error message and abort with coredump. Does not try to
      !close file, so should be used from anything xml_Close might
      !itself call (to avoid infinite recursion!)

      type(xmlf_t), intent(in) :: xf
      character(len=*), intent(in) :: msg

      write(6,'(a)') 'ERROR(wxml) in writing to file ', xmlf_name(xf)
      write(6,'(a)')  msg

      call pxfabort()
      stop

    end subroutine wxml_fatal_xf


    pure function xmlf_name(xf) result(fn)
      type (xmlf_t), intent(in) :: xf
      character(len=size(xf%filename)) :: fn
      fn = str_vs(xf%filename)
    end function xmlf_name

    pure function xmlf_opentag_len(xf) result(n)
      type (xmlf_t), intent(in) :: xf
      integer :: n

      if (xf%lun == -1) then
        n = 0
      elseif (is_empty(xf%stack)) then
        n = 0
      else
        n = len(get_top_elstack(xf%stack))
      endif
    end function xmlf_opentag_len
      
    function xmlf_opentag(xf) result(fn)
      type (xmlf_t), intent(in) :: xf
      character(len=xmlf_opentag_len(xf)) :: fn

      if (xf%lun == -1) then
        fn = ''
      elseif (is_empty(xf%stack)) then
        fn = ''
      else
        fn = get_top_elstack(xf%stack)
      endif
    end function xmlf_opentag

end module m_wxml_core
