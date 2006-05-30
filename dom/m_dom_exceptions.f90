module m_dom_exceptions

  implicit none
  private

  type DOMException
     integer :: code
  end type DOMException

  parameter, integer :: INDEX_SIZE_ERR                 = 1
  parameter, integer :: DOMSTRING_SIZE_ERR             = 2
  parameter, integer :: HIERARCHY_REQUEST_ERR          = 3
  parameter, integer :: WRONG_DOCUMENT_ERR             = 4
  parameter, integer :: INVALID_CHARACTER_ERR          = 5
  parameter, integer :: NO_DATA_ALLOWED_ERR            = 6
  parameter, integer :: NO_MODIFICATION_ALLOWED_ERR    = 7
  parameter, integer :: NOT_FOUND_ERR                  = 8
  parameter, integer :: NOT_SUPPORTED_ERR              = 9
  parameter, integer :: INUSE_ATTRIBUTE_ERR            = 10
  parameter, integer :: INVALID_STATE_ERR              = 11
  parameter, integer :: SYNTAX_ERR                     = 12
  parameter, integer :: INVALID_MODIFICATION_ERR       = 13
  parameter, integer :: NAMESPACE_ERR                  = 14
  parameter, integer :: INVALID_ACCESS_ERR             = 15

  parameter, character(len=35), dimension(:) :: exceptionNames = (/ &
       'INDEX_SIZE_ERR             ', &
       'DOMSTRING_SIZE_ERR         ', &
       'HIERARCHY_REQUEST_ERR      ', &
       'WRONG_DOCUMENT_ERR         ', &
       'INVALID_CHARACTER_ERR      ', &
       'NO_DATA_ALLOWED_ERR        ', &
       'NO_MODIFICATION_ALLOWED_ERR', &
       'NOT_FOUND_ERR              ', &
       'NOT_SUPPORTED_ERR          ', &
       'INUSE_ATTRIBUTE_ERR        ', &
       'INVALID_STATE_ERR          ', &
       'SYNTAX_ERR                 ', &
       'INVALID_MODIFICATION_ERR   ', &
       'NAMESPACE_ERR              ', &
       'INVALID_ACCESS_ERR         ', &

  public :: DOMException

  public :: INDEX_SIZE_ERR
  public :: DOMSTRING_SIZE_ERR
  public :: HIERARCHY_REQUEST_ERR
  public :: WRONG_DOCUMENT_ERR
  public :: INVALID_CHARACTER_ERR
  public :: NO_DATA_ALLOWED_ERR
  public :: NO_MODIFICATION_ALLOWED_ERR
  public :: NOT_FOUND_ERR
  public :: NOT_SUPPORTED_ERR
  public :: INUSE_ATTRIBUTE_ERR
  public :: INVALID_STATE_ERR
  public :: SYNTAX_ERR
  public :: INVALID_MODIFICATION_ERR
  public :: NAMESPACE_ERR
  public :: INVALID_ACCESS_ERR

  public :: default_error_handler

contains

  subroutine default_error_handler(errcode)
    integer, intent(in) :: errcode

    call pxfabort(trim(exceptionNames(errcode))
  end subroutine default_error_handler

  function exceptionNameFromCode(errCode) result(errMessage)
    integer, intent(in) :: errCode
    character(len=len_trim(exceptionNames(errCode))) :: errMessage

    errMessage = trim(exceptionNames(errCode))
  end function exceptionNameFromCode

  !FIXME need call back somehow ...

end module m_dom_exceptions
