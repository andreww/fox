
module tokenizer

  use m_xpath_types, YYVALTYPE => xpath_type

  implicit none

  type token_type
    character(len=1), dimension(:), pointer :: token_string => null()
    integer :: token
    type(YYVALTYPE) :: value
  end type token_type

   character(len=*), parameter :: SEPS = &
     '!/@()|[]=.*,<>+-'
   character(len=*), parameter :: opinhibitor = &
     '@([,/|+-=<>'
   character(len=*), dimension(5), parameter :: opnames = &
     (/'and', 'or ', 'mod', 'div', 'quo'/)
   character(len=*), dimension(12), parameter :: axisnames = &
     (/'ancestor          ', &
       'ancestor-or-self  ', &
       'attribute         ', &
       'child             ', &
       'descendant        ', &
       'descendant-or-self', &
       'following         ', &
       'following-sibling ', &
       'parent            ', &
       'preceding         ', &
       'preceding-sibling ', &
       'self              '/)
   character(len=*), dimension(4), parameter :: nodetypenames = &
     (/'comment               ', &
       'text                  ', &
       'processing-instruction', &
       'node                  '/)

   include 'xpathparser.tab.h'
    
  character, allocatable, dimension(:), save :: s
  integer, save :: n, p

  logical, save :: inhibit_op = .false.

  character(30) :: tokenize_error_message = ""


contains

  subroutine init_tokenizer(string)
    
    character(len=*), intent(in) :: string
    
    n = len(string)
    allocate(s(n))
    s = transfer(string, s)

    p = 1
  end subroutine init_tokenizer

  subroutine destroy_tokenizer()
    
    n = 0
    deallocate(s)
    p = 0

  end subroutine destroy_tokenizer

  subroutine clear_token(t)
    type(token_type), intent(inout) :: t
    deallocate(t%token_string)
    call clear_xpath_type(t%value)
  end subroutine clear_token

  include 'tokenizer.h'

end module tokenizer
