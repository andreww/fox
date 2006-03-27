
 function next_token() result(token)
   type(token_type) :: token


   integer :: i, tsp

   call skip_blank()

   if (p>=n) then
     token%token = 0
     return
   endif

   is_number: if (match_number_len(p) > 0) then
     print*,"tohw:NUMBER"
     token = token_construct(NUMBER, match_number(p), number=match_number(p))
     p = p + match_number_len(p)
     return
   else
     ! is it a double colon?
     if (s(p) == ':' .and. p < n-1) then
       if (s(p+1)==':') then
         print*,"tohw:AXISSUF"
         token = token_construct(AXISSUF, '::')
         p = p + 2
         return
       endif
     endif
     if (scan(s(p), SEPS) > 0) then
       if (s(p) == '.' .and. s(p+1) == '.') then
         print*,"tohw:PARENT"
         token = token_construct(PARENT, '..')
         p = p + 2
         return
       elseif (s(p) == '<' .and. s(p+1) == '=') then
         print*,"tohw:LE"
         token = token_construct(LE, '<=')
         p = p + 2
         return
       elseif (s(p) == '>' .and. s(p+1) == '=') then
         print*,"tohw:GE"
         token = token_construct(GE, '>=')
         p = p + 2
         return
       elseif (s(p) == '!' .and. s(p+1) == '=') then
         print*,"tohw:NE"
         token = token_construct(NE, '!=')
         p = p + 2
         return
       else
         if (s(p)=='*') then
           if (inhibit_op) then
             inhibit_op = .false.
             print*,"tohw:WILDCARDNAME"
             token = token_construct(WILDCARDNAME, '*')
             p = p + 1
             return
           else
             inhibit_op = .true.
             print*,"tohw:MULTIPLYOPERATOR"
             token = token_construct(MULTIPLYOPERATOR, '*')
             p = p + 1
             return
           endif
         else
           inhibit_op = (scan(s(p), opinhibitor) > 0)
           print*,"tohw:separator"
           token = token_construct(iachar(s(p)), s(p))
           p = p + 1
           return
         endif
       endif
     else if (s(p)=='$') then
       ! a variable reference
       if (match_qname_len(p+1) == 0) then
         tokenize_error_message = "Unrecognized token"
         token%token = -1
         return
       else
         print*,"tohw:VARIABLEREFERENCE"
         token = token_construct(VARIABLEREFERENCE, '$'//match_qname(p+1), variable=match_qname(p+1))
         p = p + 1 + match_qname_len(p+1)
         return
       endif
     else !name or literal
       is_qname: if (match_qname_len(p) > 0) then
         tsp = p + match_qname_len(p)
         ! a name: WildcardName, NodeType, Operator, AxisIdentifier, function name
         if (.not.inhibit_op) then
           i = isIn(match_qname(tsp), opnames)
           if (i > 0) then
             inhibit_op = .true.
             print*,"tohw:operator"
             token = token_construct(1, trim(opnames(i)), name=trim(opnames(i)))
             p = p + len_trim(opnames(i))
             return
           endif
         endif
         inhibit_op = .false.
         if (match_followsParen_len(tsp) > 0) then
           i = isIn(match_qname(tsp), nodetypenames)
           if (i > 0) then
             print*,"tohw:nodetypename"
             token = token_construct(1, trim(nodetypenames(i)), name=trim(nodetypenames(i)))
             p = p + len_trim(nodetypenames(i))
             return
           else
             ! function name. Resolve function
             print*,"tohw:functionname"
             token = token_construct(FUNCTIONNAME, match_qname(p), function=match_qname(tsp))
             p = p + match_qname_len(p)
             return
           endif
         else
           i = isIn(match_qname(p), axisnames)
           if (match_followsAxisSuf_len(p) > 0 .and. i > 0) then
             print*,"tohw:axisname"
             token = token_construct(1, trim(axisnames(i)), name=trim(axisnames(i)))
             p = p + len_trim(axisnames(i))
             return
           else
             ! a QName as WildcardName
             inhibit_op = .false.
             i = index(match_qname(p), ':')
             if (i==0) then
               print*,"tohw:qname"
               token = token_construct(1, match_qname(p), namespace='', localname=match_qname(p))
               p = p + match_qname_len(p)
               return
             else
               print*,"tohw:ncname"
               token = token_construct(1, match_qname(p), namespace=match_ncname(p), localname=match_ncname(p+i+1))
               p = p + match_qname_len(p)
               return
               !               token = token_construct(WILDCARDNAME, 'wtf') ! resolve namepsace?
             endif
           endif
         endif
       else 
         inhibit_op = .false.
         if (match_Wildcard_len(p) > 0) then
           !Do that wildcard thang.
           continue
         else
           if (match_Literal_len(p) > 0) then
             print*,"tohw:LITERAL"
             token = token_construct(LITERAL, match_Literal(p))
             p = p + match_Literal_len(p)
             return
           else
             tokenize_error_message = "Unrecognized token"
             token%token = -1
             return
           endif
         endif
       endif is_qname
     endif
   endif is_number

   if (tokenize_error_message /= "") then
     print*, tokenize_error_message
     print*, tokenize_error_message
     print*, "Unrecognized token!"
     token%token = -1
   endif
   
   if (.not.associated(token%token_string)) then
     print*, "shouldn't be here!!"
     token%token = -1
   endif
        
 end function next_token

   subroutine skip_blank()
     do while (p<=n)
       if (scan(s(p), WHITESPACE) == 0) exit
       p = p + 1
     enddo
   end subroutine skip_blank

! API for match_ functions:
   ! each consists of two parts;
   ! match_Xxx and match_Xxx_len
   !
   ! match_Xxx_len will return an integer
   ! which is the length of the match found,
   ! and 0 if there was no match.
   !
   ! match_Xxx will return a string which
   ! is the matched string. It will also
   ! set the global variable "grouping"
   ! to something useful. 


   function match_Number(p_) result(token)
     integer, intent(in) :: p_
     character(len=match_number_len(p_)) :: token
     integer :: i
     
     do i = 1, len(token) 
       token(i:i) = s(p_ + i - 1)
     enddo

   end function match_Number

   pure function match_Number_len(p_) result(m)
     integer, intent(in) :: p_
     integer :: m
     integer :: tsp ! token_position, temp_string_position
     if (p_ > n) then
       m = 0
       return
     endif
     tsp = p_
     do while (tsp <= n)
       if (scan(s(tsp), DIGIT) == 0) exit
       tsp = tsp + 1
     enddo
     if (tsp <= n) then
       if (s(tsp)=='.') then
         tsp = tsp + 1
       endif
     endif
     do while (tsp <= n)
       if (scan(s(tsp), DIGIT) == 0) exit
       tsp = tsp + 1
     enddo

     m = tsp - p_

   end function match_Number_len
     
   function match_Qname(p_) result(token)
     integer, intent(in) :: p_
     character(len=match_qname_len(p_)) :: token
     integer :: i
     
     do i = 1, len(token)
       token(i:i) = s(p_ + i - 1)
     enddo
     
   end function match_Qname

   pure function match_Qname_len(p_) result (m)
     integer, intent(in) :: p_
     integer :: m
     integer :: tsp, len2
     if (p_ > n) then
       m = 0
       return
     endif
     m = match_NCname_len(p_)
     if (m == 0) then
       return
     endif
     tsp = p_ + m
     if (s(tsp) == ':') then
       tsp = tsp + 1
       len2 = match_NCname_len(tsp)
       if (len2 > 0) then
         m = m + 1 + len2
       endif
     endif
   end function match_Qname_len
       
   function match_NCname(p_) result(token)
     integer, intent(in) :: p_
     character(len=match_NCname_len(p_)) :: token
     integer :: i
     
     do i = 1, len(token)
       token(i:i) = s(p_ + i - 1)
     enddo

   end function match_NCname

   pure function match_NCname_len(p_) result(m)
     integer, intent(in) :: p_
     integer :: m
     integer :: tp, tsp
     if (p_ > n) then
       m = 0
       return
     endif

     tp = 1
     tsp = p_
     if (scan(s(tsp),LETTER)>0 .or. s(tsp) == '_') then
       tsp = tsp + 1
       tp = tp + 1
       do while (tsp <= n)
         if (scan(s(tsp), NCNAMECHAR) == 0) exit
         tsp = tsp + 1
         tp = tp + 1
       enddo
     endif
     m = tp - 1
   end function match_NCname_len

   function match_followsParen(p_) result(token)
     integer, intent(in) :: p_
     character(len=match_followsParen_len(p_)) :: token
     integer :: i
     
     do i = 1, len(token)
       token(i:i) = s(p_ + i - 1)
     enddo

   end function match_followsParen
   
   pure function match_followsParen_len(p_) result(m)
     integer, intent(in) :: p_
     integer :: m
     integer :: tsp
     tsp = p_
     m = 0
     do while (tsp <= n)
       if (scan(s(tsp), WHITESPACE) == 0) exit
       tsp = tsp + 1
     enddo
     if (tsp <= n) then
       if (s(tsp) == '(') then
         m = tsp - p_ + 1
       endif
     endif
   end function match_followsParen_len

   function match_followsAxisSuf(p_) result(token)
     integer, intent(in) :: p_
     character(len=match_followsAxisSuf_len(p_)) :: token
     integer :: i
     
     do i = 1, len(token)
       token(i:i) = s(p_ + i - 1)
     enddo

   end function match_followsAxisSuf
   
   pure function match_followsAxisSuf_len(p_) result(m)
     integer, intent(in) :: p_
     integer :: m
     integer :: tsp
     tsp = p_
     do while (tsp <= n)
       if (scan(s(tsp), WHITESPACE) == 0) exit
       tsp = tsp + 1
     enddo
     if (tsp <= n-1) then
       if (s(tsp) == ':' .and. s(tsp+1) == ':') then
         m = tsp + 1
       endif
     else
       m = 0
     endif
   end function match_followsAxisSuf_len

   function match_Wildcard(p_) result(token)
     integer, intent(in) :: p_
     character(len=match_Wildcard_len(p_)) :: token
     integer :: i
     
     do i = 1, len(token)
       token(i:i) = s( p_ + i - 1)
     enddo
   end function match_Wildcard


   pure function match_Wildcard_len(p_) result(m)
     integer, intent(in) :: p_
     integer :: m
     if (p_ > n) then
       m = 0
       return
     endif
     m = match_NCname_len(p_)
     if (m > 0 .and. p_ < n - 1) then
       if (s(p_+m) == ':' .and. s(p_+m+1) == "*") then
         m = m + 2
       else 
         m = 0
       endif
     else
       m = 0
     endif
   end function match_Wildcard_len

   pure function match_Literal_len(p_) result(m)
     integer, intent(in) :: p_
     integer :: m
     integer :: tsp
     if (p_ > n) then
       m = 0
       return
     endif
     tsp = p_
     if (s(tsp) == "'") then
       tsp = tsp + 1
       do while (tsp <= n)
         if (s(tsp) == "'") exit
         tsp = tsp + 1
       enddo
       tsp = tsp + 1
     elseif (s(tsp) == '"') then
       tsp = tsp + 1
       do while (tsp <= n)
         if (s(tsp) == '"') exit
         tsp = tsp + 1
       enddo
     else
       m = 0
       return
     endif

     if (tsp > n) then
       m = 0
       return
     endif

     m = tsp - p_
   end function match_Literal_len


   function match_Literal(p_) result(token)
     integer, intent(in) :: p_
     character(len=match_Literal_len(p_)) :: token
     integer :: i
     do i = 1, len(token)
       token(i:i) = s(p_ + i - 1)
     enddo
   end function match_Literal
     

   function isIn(string, strings) result(m)
     integer :: m
     character(len=*), intent(in) :: string
     character(len=*), dimension(:) :: strings
     integer :: i
     m = 0
     do i = 1, size(strings)
       if (string == trim(strings(i))) then
         m = i
         exit
       endif
     enddo
   end function isIn
       

   function token_construct(tokenDef, tokenstring, number, name, variable, function, literal, namespace, localname) result(thisToken)
     integer, intent(in) :: tokenDef
     character(len=*), intent(in) :: tokenstring
     character(len=*), intent(in), optional :: number
     character(len=*), intent(in), optional :: name
     character(len=*), intent(in), optional :: variable
     character(len=*), intent(in), optional :: function
     character(len=*), intent(in), optional :: literal
     character(len=*), intent(in), optional :: namespace
     character(len=*), intent(in), optional :: localname

     type(token_type) :: thisToken


     thisToken%token = tokenDef
     allocate(thisToken%token_string(len(tokenstring)))
     thisToken%token_string = transfer(tokenstring, thisToken%token_string)

     if (present(number)) then
       print*,"tohw:found a number"
       read(number, *) thisToken%value%number
       thisToken%value%type = 3
     !elseif (present(variable)) then
     !  allocate(thisToken%token_string(len(variable)))
     !  thisToken%token_string = transfer(variable, thisToken%token_string)
       ! get variable value
     !elseif (present(function)) then
     !  allocate(thisToken%token_string(len(function)))
    !   thisToken%token_string = transfer(function, thisToken%token_string)
    !   ! get function reference
     endif

   end function token_construct
