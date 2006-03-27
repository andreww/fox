module m_wcml_attributes

  use m_wxml_core, only : xml_AddAttribute
  implicit none

  private

  public :: cmlAddAttribute

  contains

    subroutine cmlAddAttribute(xf, attr, value, check)
      type(xmlf_t), intent(inout) : xf
      character(len=*), intent(in) :: attr
      character(len=*), intent(in) :: value
      logical, intent(in), optional :: check

      if (present(check)) then
        if (check)  then
          if (checkAttribute('errorBasis')) then
            call pxfabort('Unknown attribute output')
          endif
        endif
      else
        if (checkAttribute('errorBasis')) then
          call pxfwarning('Unknown attribute output')
        endif
      endif

      call xml_Addattribute(xf, attr, value)

    end subroutine cmLAddAttribute
        

    function isValidFP(number) result(ok)
      character(len=*), intent(in) :: number
      logical :: ok

      integer :: i, state

      !This should implement [0-9]?\.*[0-9]*[Ee]*[0-9]?
      
      state = 0
      ok = i>0
      do i = 1, len(number)
        select case(state)
        case (0)
          if (index(number(i:i), digits) == -1) then
            if (index(number(i:i), '.')) then
              state = 1
            else
              ok = .false.
              exit
            endif
          endif
        case(1)
          if (index(number(i:i), digits) == -1) then
            if (index(number(i:i), 'Ee')) then
              state = 2
            else
              ok = .false.
              exit
            endif
          endif
        case(2)
          if (index(number(i:i), digits) == -1) then
            ok = .false.
            exit
          else
            state = 3
          endif
        case(3)
          if (index(number(i:i), digits) == -1) then
            ok = .false.
            exit
          endif
        end select
      end do
        
      if (state==2) ok = .false.
        
      return ok
  
    end function isValidFP

    function isValidQName

    function checkAttributeValue(attr, value) result(ok)
      character(len=*), intent(in) :: attr
      character(len=*), intent(in) :: value
      logical :: ok

      !FIXME some of these are probably required to be QNames

      ok = .false.
      select case(attr)
        case ('id'):
          ok = .true.
        case ('errorBasis'):
          if (value == 'observedRange' .or. &
              value == 'observedStandardDeviation' .or. &
              value == 'observedStandardError' .or. &
              value == 'estimatedStandardDeviation' .or. &
              value == 'estimatedStandardError') &
             ok = .true.
        case ('title')
          ok = .true.
        case ('dictRef')
          ok = .true.
        case ('dataType')
          ok = .true. !FIXME
        case ('convention')
          ok = .true.
        case ('errorValue')
          ok = isValidFP(value)
        case ('min')
          ok = isValidFP(value)
        case ('max')
          ok = isValidFP(value)
        case ('ref')
          ok = .true.
        case ('units')
          ok = .true.
        end select
