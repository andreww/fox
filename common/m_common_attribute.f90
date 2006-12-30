module m_common_attribute

  ! Structure and manipulation of attribute specification

  implicit none
  private

  integer, parameter :: ATT_NULL = 0

  integer, parameter :: ATT_CDATA = 1
  integer, parameter :: ATT_ID = 2
  integer, parameter :: ATT_IDREF = 3
  integer, parameter :: ATT_IDREFS = 4
  integer, parameter :: ATT_ENTITY = 5
  integer, parameter :: ATT_ENTITIES = 6
  integer, parameter :: ATT_NMTOKEN = 7
  integer, parameter :: ATT_NMTOKENS = 8
  integer, parameter :: ATT_NOTATION = 9
  integer, parameter :: ATT_ENUM = 10

  integer, parameter :: ATT_REQUIRED = 1
  integer, parameter :: ATT_DEFAULT = 2
  integer, parameter :: ATT_FIXED = 3

  type string_t
    ! annoyingly ...
    character, pointer :: s(:) => null()
  end type string_t

  type attribute_t
    character, pointer :: name(:) => null()
    integer :: attType = 0 
    integer :: attDefault = 0 
    type(string_t), pointer :: enumerations => null()
    character, pointer :: default(:) => null()
  end type attribute_t

  public :: attribute_t
  public :: parse_dtd_attribute

contains

  subroutine parse_dtd_attribute(contents, stack, elementName, element)
    character(len=*), intent(in) :: contents
    type(error_stack), intent(inout) :: stack
    character(len=*), intent(in) :: elementName
    type(element_t), intent(inout) :: el

    integer :: i

    do i = 1, len(contents) + 1
      if (i<=len(contents)) then
        c = contents(i:i)
      else
        c = ' '
      endif

    parsety-parse

  enddo

  end subroutine parse_dtd_attribute

end module m_common_attribute
