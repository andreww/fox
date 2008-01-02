module m_sax_types

  use m_common_attrs, only: dictionary_t
  use m_common_elstack, only: elstack_t
  use m_common_entities, only: entity_list
  use m_common_error, only: error_stack
  use m_common_namespaces, only: namespacedictionary
  use m_common_notations, only: notation_list
  use m_common_struct, only: xml_doc_state

  use m_sax_reader, only: file_buffer_t

  implicit none

  ! Context

  integer, parameter :: CTXT_NULL = -1
  integer, parameter :: CTXT_INIT = 0
  integer, parameter :: CTXT_BEFORE_DTD = 1
  integer, parameter :: CTXT_IN_DTD = 2
  integer, parameter :: CTXT_IGNORE = 3
  integer, parameter :: CTXT_BEFORE_CONTENT = 4
  integer, parameter :: CTXT_IN_CONTENT = 5
  integer, parameter :: CTXT_AFTER_CONTENT = 6

  ! State

  integer, parameter :: ST_STOP = -2
  integer, parameter :: ST_NULL = -1
  integer, parameter :: ST_MISC = 0
  integer, parameter :: ST_BANG_TAG = 1 
  integer, parameter :: ST_START_PI = 2
  integer, parameter :: ST_START_COMMENT = 3
  integer, parameter :: ST_START_TAG = 4 
  integer, parameter :: ST_START_SECTION_DECLARATION = 5
  integer, parameter :: ST_FINISH_CDATA_DECLARATION = 6
  integer, parameter :: ST_FINISH_SECTION_DECLARATION = 7
  integer, parameter :: ST_IN_IGNORE_SECTION = 8
  integer, parameter :: ST_IN_TAG = 9
  integer, parameter :: ST_ATT_NAME = 10
  integer, parameter :: ST_ATT_EQUALS = 11
  integer, parameter :: ST_CHAR_IN_CONTENT = 12
  integer, parameter :: ST_CLOSING_TAG = 13
  integer, parameter :: ST_PI_END = 14
  integer, parameter :: ST_COMMENT_END = 15
  integer, parameter :: ST_PI_CONTENTS = 16
  integer, parameter :: ST_CDATA_CONTENTS = 17
  integer, parameter :: ST_IN_CLOSING_TAG = 18
  integer, parameter :: ST_TAG_IN_CONTENT = 19
  integer, parameter :: ST_CDATA_END = 20
  integer, parameter :: ST_IN_DTD = 21
  integer, parameter :: ST_DTD_NAME = 22
  integer, parameter :: ST_DTD_SYSTEM = 23
  integer, parameter :: ST_DTD_PUBLIC = 24
  integer, parameter :: ST_SUBSET = 25
  integer, parameter :: ST_DTD_ATTLIST = 26
  integer, parameter :: ST_DTD_ELEMENT = 27
  integer, parameter :: ST_DTD_ENTITY= 28
  integer, parameter :: ST_DTD_NOTATION = 29
  integer, parameter :: ST_DTD_NOTATION_ID = 30
  integer, parameter :: ST_DTD_NOTATION_SYSTEM = 31
  integer, parameter :: ST_DTD_NOTATION_PUBLIC = 32
  integer, parameter :: ST_DTD_NOTATION_PUBLIC_2 = 33
  integer, parameter :: ST_DTD_NOTATION_END = 34
  integer, parameter :: ST_DTD_DECL = 35
  integer, parameter :: ST_CLOSE_DTD = 36
  integer, parameter :: ST_DTD_ENTITY_PE = 37
  integer, parameter :: ST_DTD_ENTITY_ID = 38
  integer, parameter :: ST_DTD_ENTITY_PUBLIC = 39
  integer, parameter :: ST_DTD_ENTITY_SYSTEM = 40
  integer, parameter :: ST_DTD_ENTITY_NDATA = 41
  integer, parameter :: ST_DTD_ENTITY_NDATA_VALUE = 42
  integer, parameter :: ST_DTD_ENTITY_END = 43
  integer, parameter :: ST_DTD_ATTLIST_CONTENTS = 44
  integer, parameter :: ST_DTD_ATTLIST_END = 45
  integer, parameter :: ST_DTD_ELEMENT_CONTENTS = 46
  integer, parameter :: ST_DTD_ELEMENT_END = 47
  integer, parameter :: ST_START_ENTITY = 48
  integer, parameter :: ST_START_PE = 49
  
! token types

  integer, parameter :: TOK_NULL = 0
  integer, parameter :: TOK_PI_TAG = 1 ! <?
  integer, parameter :: TOK_BANG_TAG = 2 ! <!
  integer, parameter :: TOK_OPEN_TAG = 3 ! <
  integer, parameter :: TOK_OPEN_SB = 4 ! [
  integer, parameter :: TOK_CLOSE_SB = 5 ! [
  integer, parameter :: TOK_OPEN_COMMENT = 6 ! --
  integer, parameter :: TOK_NAME = 7 ! name (+token)
  integer, parameter :: TOK_CHAR = 8 ! character data (+token)
  integer, parameter :: TOK_PI_END = 9 ! ?>
  integer, parameter :: TOK_COMMENT_END = 10 ! -->
  integer, parameter :: TOK_SECTION_START = 11 ! <![
  integer, parameter :: TOK_SECTION_END = 12 ! ]]>
  integer, parameter :: TOK_END_TAG = 13 ! >
  integer, parameter :: TOK_END_TAG_CLOSE = 14 ! />
  integer, parameter :: TOK_CLOSE_TAG = 15 ! </
  integer, parameter :: TOK_ENTITY = 16 ! % or &
  integer, parameter :: TOK_EQUALS = 17 ! =
  integer, parameter :: TOK_DTD_CONTENTS = 18 ! for element and attlist
  
  type sax_parser_t
    type(xml_doc_state), pointer :: xds
    logical :: xds_used = .false. ! is the xds used by DOM? If so, we must
                                  ! not destroy it once we are finished
    integer :: context 
    integer :: state = ST_NULL
    logical :: well_formed = .false.
    logical :: skippedExternal = .false.
    character, dimension(:), pointer :: token => null()
    integer :: tokenType = TOK_NULL
    integer :: nextTokenType = TOK_NULL
    character, dimension(:), pointer :: name => null()
    character, dimension(:), pointer :: attname => null()
    logical :: error = .false.
    type(error_stack) :: error_stack
    ! Aspects of document structure
    character, dimension(:), pointer :: root_element => null()
    type(elstack_t) :: elstack
    type(dictionary_t) :: attributes
    type(namespacedictionary) :: nsdict
    type(notation_list) :: nlist
    type(entity_list) :: predefined_e_list
    type(entity_list) :: forbidden_pe_list
    type(entity_list) :: forbidden_ge_list
    character(len=1), dimension(:), pointer :: PublicId => null()
    character(len=1), dimension(:), pointer :: SystemId => null()
    character(len=1), dimension(:), pointer :: Ndata => null()
    logical :: inIntSubset = .false.
  end type sax_parser_t


  type xml_t
    type(file_buffer_t) :: fb
    type(sax_parser_t) :: fx
  end type xml_t


end module m_sax_types
