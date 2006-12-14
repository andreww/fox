module m_sax_types

  use m_common_entities, only: entity_list

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
    character, dimension(:), pointer :: msg
  end type sax_error_t

  type sax_parser_t
    logical :: discard_whitespace = .false.
    logical :: long_token = .false.
    integer :: context 
    integer :: state
    integer :: parse_stack = 0
    logical :: well_formed = .false.
    type(dtd_parser_t) :: dtd_parser
    character, dimension(:), pointer :: token => null()
    type(sax_error_t), dimension(:), pointer :: error_stack => null()
    integer :: xml_version
  end type sax_parser_t


end module m_sax_types
