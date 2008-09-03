module m_wncml_util

  use netcdf
  
  !, only : NF90_BYTE, NF90_CHAR, NF90_SHORT, NF90_INT, NF90_FLOAT, &
  !               &     NF90_DOUBLE !! , NF90_UBYTE, NF90_UINT, NF90_INT64, &
               !!  &     NF90_STRING
               !! THese are not in the libary on my machine.

  implicit none

contains 

  function getType ( xtype )

   
    integer,                    intent(in   ) :: xtype
    character(len=1000)                       :: getType


    select case (xtype)
    case (NF90_BYTE)
      getType='byte'
    case (NF90_CHAR)
      getType='char'
    case (NF90_SHORT)
      getType='short'
    case (NF90_INT)
      getType='int'
    case (NF90_FLOAT)
      getType='float'
    case (NF90_DOUBLE)
      getType='double'
!    case (NF90_UBYTE)
!      stop 'Type NF90_UBYTE  Not implemented'
!    case (NF90_USHORT)
!      stop 'Type NF90 USHORT Not implemented'
!    case (NF90_UINT)
!      stop 'Type NF90 UINT Not implemented'
!    case (NF90_INT64)
!      stop 'Type NF90 INT64 Not implemented'
!    case (NF90_STRING)
!      getType='string'
    case default
      stop 'Unlknown and user defined types are not implemented'
    end select

  end function getType


  subroutine check(code)

    integer, intent(in) :: code

    ! FIXME: Should hook into FoX's error reporting tools
    !        rather than stopping

    if (code /= nf90_noerr) print*,  NF90_STRERROR(code)
    if (code /= nf90_noerr) stop "netCDF error: "

  end subroutine check

  function namedims (ncid, ndims, DimIds)
    integer, parameter  :: MAX_NAME_LEN = 100
    integer, intent(in) :: ncid
    integer, intent(in) :: ndims
    integer, intent(in) :: DimIds(ndims)
    character(len=MAX_NAME_LEN) :: thisdim
    character(len=MAX_NAME_LEN*NF90_MAX_DIMS+NF90_MAX_DIMS) :: namedims
    integer :: i, len

    namedims = ''
    do i = 1, ndims
      call check ( NF90_INQUIRE_DIMENSION(ncid, DimIds(i), thisdim, len) )
      namedims = trim(namedims) // ' ' // trim(thisdim)
    enddo 

    namedims = adjustl(namedims)

  end function namedims

end module m_wncml_util
