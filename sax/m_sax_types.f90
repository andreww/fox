module m_sax_types

  use m_common_attrs, only: dictionary_t
  use m_common_elstack, only: elstack_t
  use m_common_entities, only: entity_list
  use m_common_error, only: error_stack
  use m_common_namespaces, only: namespacedictionary
  use m_common_notations, only: notation_list

  use m_sax_reader, only: file_buffer_t

  implicit none

  ! Context

  integer, parameter :: CTXT_NULL = -1
  integer, parameter :: CTXT_INIT = 0
  integer, parameter :: CTXT_BEFORE_DTD = 1
  integer, parameter :: CTXT_IN_DTD = 2
  integer, parameter :: CTXT_BEFORE_CONTENT = 3
  integer, parameter :: CTXT_IN_CONTENT = 4
  integer, parameter :: CTXT_AFTER_CONTENT = 5

  ! State

  integer, parameter :: ST_NULL = -1
  integer, parameter :: ST_MISC = 0
  integer, parameter :: ST_BANG_TAG = 1 
  integer, parameter :: ST_START_PI = 2
  integer, parameter :: ST_START_COMMENT = 3
  integer, parameter :: ST_START_TAG = 4 
  integer, parameter :: ST_START_CDATA_1 = 5
  integer, parameter :: ST_START_CDATA_2 = 6
  integer, parameter :: ST_IN_TAG = 7
  integer, parameter :: ST_ATT_NAME = 8
  integer, parameter :: ST_ATT_EQUALS = 9
  integer, parameter :: ST_CHAR_IN_CONTENT = 10
  integer, parameter :: ST_CLOSING_TAG = 11
  integer, parameter :: ST_PI_END = 13
  integer, parameter :: ST_COMMENT_END_1 = 14
  integer, parameter :: ST_COMMENT_END_2 = 15
  integer, parameter :: ST_PI_CONTENTS = 16
  integer, parameter :: ST_CDATA_CONTENTS = 17
  integer, parameter :: ST_IN_CLOSING_TAG = 18
  integer, parameter :: ST_TAG_IN_CONTENT = 19
  integer, parameter :: ST_CDATA_END = 20
  integer, parameter :: ST_IN_DTD = 21
  integer, parameter :: ST_DTD_NAME = 22
  integer, parameter :: ST_DTD_SYSTEM = 23
  integer, parameter :: ST_DTD_PUBLIC = 24
  integer, parameter :: ST_INT_SUBSET = 25
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
  
  ! Whitespace
  
  integer, parameter :: WS_FORBIDDEN = 0
  integer, parameter :: WS_PRESERVE = 1
  integer, parameter :: WS_MANDATORY = 2
  integer, parameter :: WS_DISCARD = 3
  
  type sax_parser_t
    integer :: whitespace
    integer :: context 
    integer :: state
    integer :: parse_stack = 0
    logical :: well_formed = .false.
    character, dimension(:), pointer :: token => null()
    character, dimension(:), pointer :: next_token => null()
    character, dimension(:), pointer :: name => null()
    character, dimension(:), pointer :: attname => null()
    logical :: error = .false.
    logical :: pe = .false.
    type(error_stack) :: error_stack 
    ! Aspects of document structure
    integer :: xml_version
    character, dimension(:), pointer :: encoding => null()
    logical :: standalone
    character, dimension(:), pointer :: root_element => null()
    type(elstack_t) :: elstack
    type(dictionary_t) :: attributes
    type(namespacedictionary) :: nsdict
    type(notation_list) :: nlist
    type(entity_list) :: pe_list
    type(entity_list) :: ge_list
    character(len=1), dimension(:), pointer :: PublicId => null()
    character(len=1), dimension(:), pointer :: SystemId => null()
    character(len=1), dimension(:), pointer :: Ndata => null()
    logical :: skippedExternalEntity = .false.
    logical :: processDTD = .true.
  end type sax_parser_t


  type xml_t
    type(file_buffer_t) :: fb
    type(sax_parser_t) :: fx
    integer :: sax_signal
  end type xml_t


end module m_sax_types
