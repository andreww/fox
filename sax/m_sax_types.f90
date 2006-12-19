module m_sax_types

  use m_common_elstack, only: elstack_t
  use m_common_entities, only: entity_list

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
  integer, parameter :: ST_IN_PI = 12
  integer, parameter :: ST_PI_END = 13
  integer, parameter :: ST_COMMENT_END_1 = 14
  integer, parameter :: ST_COMMENT_END_2 = 15
  integer, parameter :: ST_PI_CONTENTS = 16
  integer, parameter :: ST_CDATA_CONTENTS = 17
  integer, parameter :: ST_IN_CLOSING_TAG = 18
  integer, parameter :: ST_TAG_IN_CONTENT = 19
  
  type dtd_parser_t
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
    type(entity_list) :: entity_list
    integer :: dtd_state
    logical :: external_found
    logical :: parameter_entity
    logical :: internal_subset
  end type dtd_parser_t

  type sax_error_t
    character, dimension(:), pointer :: msg => null()
  end type sax_error_t

  type sax_parser_t
    logical :: discard_whitespace = .false.
    integer :: context 
    integer :: state
    integer :: parse_stack = 0
    logical :: well_formed = .false.
    type(dtd_parser_t) :: dtd_parser
    character, dimension(:), pointer :: token => null()
    character, dimension(:), pointer :: next_token => null()
    character, dimension(:), pointer :: name => null()
    logical :: error = .false.
    type(sax_error_t), dimension(:), pointer :: error_stack => null()
    ! Aspects of document structure
    integer :: xml_version
    character, dimension(:), pointer :: encoding => null()
    logical :: standalone
    character, dimension(:), pointer :: root_element => null()
    type(elstack_t) :: elstack
  end type sax_parser_t


  type xml_t
    type(file_buffer_t) :: fb
    type(sax_parser_t) :: fx
    integer :: sax_signal
  end type xml_t


end module m_sax_types
