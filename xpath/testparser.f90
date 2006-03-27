program testparser
  use tokenizer, only : init_tokenizer, destroy_tokenizer
  use parse_bison90, only : yyparse, yylval, YYACPT, YYABRT

  use m_xpath_types, only : xpath_type, print_xpath

  character(len=*), parameter, dimension(1) :: teststrings = &
     (/           &
       "123                                                                                     " &
 !     ,"'abc'                                                                                   " &       
 !     ,"count(//form) = 1                                                                       " &
 !     ,"//verify[@stepid='a1']                                                                  " &
 !     ,"123.345(kjhkjh_:lkjl:kj:*'ljhg""ljhg'ljk)                                                " &
 !     ,"tow:min($pointSet/pointList/point/@x)                                                   " &
 !     ,"not(parent::cml:parameterList)                                                          " &
 !     ,"count(ancestor::cml:step[//cml:molecule]/preceding-sibling::cml:step[//cml:molecule])+1 " &
 !     ,"child::*/@units                                                                         " &
     /)

  type(xpath_type) :: parse_result 
  integer :: r


  do i = 1, size(teststrings)
    call init_tokenizer(teststrings(i))
    r = yyparse()
    select case (r)
      case (YYACPT)
        ! super, it worked
        parse_result = yylval
        call print_xpath(parse_result)
      case (YYABRT)
        ! what a shame
        print*,"Error parsing Xpath expression:" // teststrings(i)
    end select
    call destroy_tokenizer()
  enddo

end program testparser
