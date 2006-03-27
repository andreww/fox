module m_wxml_escape

implicit none

private

public  :: check_Name

character(len=*), parameter :: LETTER = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
character(len=*), parameter :: DIGIT  = '1234567890'
character(len=*), parameter :: ESCAPE = '&<'//'"'//"'"

CONTAINS

function check_Name(name) result(good)
 character(len=*), intent(in) :: name
 logical :: good
! Validates a string against the XML requirements for a NAME
! Is not fully compliant; ignores UTF issues.

 integer :: n, i

 good=.true.

 n = len(name)
 if (n == 0) then
   write(0,*) "tagName is an empty string."
   good=.false.
 elseif (good) then
   if (scan(name(1:1), LETTER//'_'//':') == 0) then
     write(0,*) "tagName must begin with a letter, underscore or colon: '", name(1:1), "'."
     good=.false.
   endif
 elseif (good) then
   do i = 1, n
     if (scan(name(i:i), LETTER//DIGIT//'.'//'-'//'_'//':') == 0) then
       write(0,*) "tagName contains a forbidden character: '", name(i:i), "'."
       good=.false.
       exit
     endif
   enddo
 elseif (good) then
   if (scan(name(1:1), 'Xx') == 1 .and. &
       scan(name(2:2), 'Mm') == 1 .and. &
       scan(name(3:3), 'Ll') == 1) then
     write(0,*) "tagName cannot start with the characters 'XML'."
     good=.false.
   endif
 endif
       
end function check_Name

end module m_wxml_escape
