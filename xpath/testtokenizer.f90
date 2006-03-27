program test_tokenizer
  use tokenizer

  character(len=*), parameter, dimension(7) :: teststrings = &
    (/"123.345(kjhkjh_:lkjl:kj:*'ljhg""ljhg'ljk)                                                " &
     ,"count(//form) = 1                                                                       " &
     ,"//verify[@stepid='a1']                                                                  " &
     ,"tow:min($pointSet/pointList/point/@x)                                                   " &
     ,"not(parent::cml:parameterList)                                                          " &
     ,"count(ancestor::cml:step[//cml:molecule]/preceding-sibling::cml:step[//cml:molecule])+1 " &
     ,"child::*/@units                                                                         " /)      

  type(token_type) :: t

  do i = 1, 7
    call init_tokenizer(teststrings(i))
    do while (.true.)
      t = next_token()
      if (t%token == -1) then
        print*,"syntax error"
        exit
      elseif (t%token == -10) then
        exit
      endif
      print*,t%token_string
      deallocate(t%token_string)
    enddo
    call destroy_tokenizer()
  enddo

  !print*,"numbers"
  !call printtest(1, match_number_len)
  !call printtest(2, match_number_len)
  !call printtest(3, match_number_len)
  !call printtest(4, match_number_len)
  !print*,"ncnames"
  !call printtest(5, match_ncname_len)
  !call printtest(6, match_ncname_len)
  !call printtest(7, match_ncname_len)
  !call printtest(8, match_ncname_len)
  !call printtest(9, match_ncname_len)
  !call printtest(10, match_ncname_len)
  !call printtest(11, match_ncname_len)
  !call printtest(12, match_ncname_len)
  !print*,"qnames"
  !call printtest(13, match_qname_len)
  !call printtest(14, match_qname_len)
  !call printtest(15, match_qname_len)
  !call printtest(16, match_qname_len)
  !call printtest(17, match_qname_len)
  !call printtest(18, match_qname_len)
  !print*,"wildcards"
  !call printtest(19, match_wildcard_len)
  !call printtest(20, match_wildcard_len)
  !call printtest(21, match_wildcard_len)
  !call printtest(22, match_wildcard_len)
  !call printtest(23, match_wildcard_len)
  !call printtest(24, match_wildcard_len)
  !print*,"literals"
  !call printtest(25, match_literal_len)
  !call printtest(26, match_literal_len)
  !call printtest(27, match_literal_len)
  !call printtest(28, match_literal_len)
  !call printtest(29, match_literal_len)
  !call printtest(30, match_literal_len)
  !call printtest(31, match_literal_len)
  !call printtest(32, match_literal_len)
  !call printtest(33, match_literal_len)

  contains

  subroutine printtest(i, func)
    integer, intent(in) :: i
    integer, external :: func
    integer :: l
    l =  func(i)
    print('(a)'), teststring
    if (l == 0) then
      print*
    else if (l==1) then
      print('(a)'), repeat(' ', i-1) // '^'
    else
      print('(a)'), repeat(' ', i-1) // '^' // repeat(' ', l-2) //'^' 
    endif

  end subroutine printtest

end program test_tokenizer
