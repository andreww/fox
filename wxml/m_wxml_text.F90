module m_wxml_text

  implicit none
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)

  interface str
    module procedure  str_integer_fmt, str_integer, &
                      str_logical_fmt, str_logical, &
                      str_real_dp_no_fmt, str_real_dp_with_fmt, &
                      str_real_sp_no_fmt, str_real_sp_with_fmt
  end interface

  public :: str

contains 

#ifndef PGF90
      elemental &
#endif
      function str_integer_fmt(i,format) result(s)
      integer, intent(in)   :: i
      character(len=*), intent(in) :: format
      character(len=100)    :: s

      write(s,format) i
      s = adjustl(s)
      end function str_integer_fmt

#ifndef PGF90
      pure &
#endif 
      function str_integer(i) result(s)
        ! This will work correctly (return an appropriately-sized
        ! string) for integers i s.t. -99999999<=i<=999999999
        integer, intent(in) :: i
#ifndef WXML_INIT_FIX
        character(len=int(merge(log10(real(max(abs(i),1)))+2, &
                                log10(real(max(abs(i),1)))+1, &
                              i<0))) :: s
#else
! Some compilers have trouble with the above
        character(len=int(log10(real(max(abs(i),1)))+2))  :: s
#endif
        character(len=4) :: form
        
        write(form,'(a,i1,a)') '(i',len(s),')'
        write(s, form) i

      end function str_integer

#ifndef PGF90
      elemental &
#endif 
      function str_logical_fmt(l,format) result(s)
      logical, intent(in)   :: l
      character(len=*), intent(in) :: format
      character(len=100)    :: s

      write(s,format) l
      s = adjustl(s)

      end function str_logical_fmt

#ifndef PGF90
      pure &
#endif 
      function str_logical(l) result(s)
        logical, intent(in)   :: l
        character(len=merge(4,5,l)) :: s
        
        if (l) then
          s='true'
        else
          s='false'
        endif
      end function str_logical

! In order to convert real numbers to strings, we need to
! perform an internal write - but how long will the 
! resultant string be? We don't know & there is no way
! to discover for an arbitrary format. Therefore, 
! (if we have the capability; f95 or better)
! we assume it will be less than 100 characters, write
! it to a string of that length, then remove leading &
! trailing whitespace. (this means that if the specified
! format includes whitespace, this will be lost.)
!
! If we are working with an F90-only compiler, then
! we cannot do this trick - the output string will
! always be 100 chars in length, though we will remove
! leading whitespace. 


#ifndef PGF90
     pure &
#endif 
     function str_real_dp_no_fmt(x) result(s)
        real(kind=dp), intent(in)   :: x
#if !defined(WXML_INIT_FIX) && !defined(PGF90)
        character(len=str_real_dp_len(x)) :: s
#else
        character(len=100)    :: s
#endif
        character(len=100)    :: s_temp
        write(s_temp,"(g22.12)") x
        s = trim(adjustl(s_temp))
      end function str_real_dp_no_fmt

#ifndef PGF90
      pure &
#endif 
      function str_real_dp_with_fmt(x, format) result(s)
        real(kind=dp), intent(in)   :: x
        character(len=*), intent(in)  :: format
#if !defined(WXML_INIT_FIX) && !defined(PGF90)
        character(len=str_real_dp_len(x)) :: s
#else
        character(len=100)    :: s
#endif
        character(len=100)    :: s_temp
        
        write(s_temp,format) x
        s = trim(adjustl(s_temp))
      end function str_real_dp_with_fmt

#ifndef PGF90
      pure &
#endif 
      function str_real_sp_no_fmt(x) result(s)
        real(kind=sp), intent(in)   :: x
#if !defined(WXML_INIT_FIX) && !defined(PGF90)
        character(len=str_real_sp_len(x)) :: s
#else
        character(len=100)    :: s
#endif
        character(len=100)    :: s_temp
        write(s_temp,"(g22.12)") x
        s = trim(adjustl(s_temp))
      end function str_real_sp_no_fmt

#ifndef PGF90
      pure &
#endif 
      function str_real_sp_with_fmt(x, format) result(s)
        real(kind=sp), intent(in)   :: x
        character(len=*), intent(in)  :: format
#if !defined(WXML_INIT_FIX) && !defined(PGF90)
        character(len=str_real_sp_len(x, format)) :: s
#else
        character(len=100)    :: s
#endif
        character(len=100)    :: s_temp
        
        write(s_temp,format) x
        s = trim(adjustl(s_temp))
      end function str_real_sp_with_fmt
      
#ifndef PGF90
      elemental &
#endif 
      function str_real_dp_len(x, format) result(n)
        real(kind=dp), intent(in) :: x
        character(len=*), intent(in), optional :: format
        character(len=100)    :: s
        integer :: n

        if (present(format)) then
          write(s,format) x
        else
          write(s,"(g22.12)") x
        endif
        s = adjustl(s)
        n = len_trim(s)
      end function str_real_dp_len
      
#ifndef PGF90
      elemental &
#endif 
      function str_real_sp_len(x, format) result(n)
        real(kind=sp), intent(in) :: x
        character(len=*), intent(in), optional :: format
        character(len=100)    :: s
        integer :: n

        if (present(format)) then
          write(s,format) x
        else
          write(s,"(g22.12)") x
        endif
        s = adjustl(s)
        n = len_trim(s)
      end function str_real_sp_len

end module m_wxml_text
