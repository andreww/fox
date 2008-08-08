module wncml

  use netcdf
  use m_wncml_xml
  use m_wncml_util, only : check, namedims
  use FoX_wxml

  implicit none
  private
  public :: ncmlDumpContainer

contains
      
  subroutine ncmlDumpContainer( xf, ncid, path )

    type(xmlf_t),     intent(inout) :: xf
    integer,          intent(in) :: ncid
    character(len=*), intent(in) :: path

  integer :: ndim, nvar, natt
  integer :: len
  integer :: i, j
  character(len=1000) ::  name, atname, str
  integer :: xtype, ndims, natts
  integer, dimension(nf90_max_var_dims) :: DimIds

    call ncmlStartContainer( xf, path)

    call check( NF90_INQUIRE(ncid, ndim, nvar, natt) )
!
! Handle each dimension
! FIXME: What about varying dimensions
!
    do i = 1, ndim
       call check ( NF90_INQUIRE_DIMENSION(ncid, i, name, len) )
       call ncmlAddDimension( xf, trim(name), len )
    enddo
!
! Handle any optional global attributes
!
    if ( natt > 0 ) then
      do i = 1, natt
        call check( NF90_INQ_ATTNAME(ncid, i, j, atname) )
        call check( NF90_INQUIRE_ATTRIBUTE(ncid, i, trim(atname), xtype, len) )
          ! FIXME: ncmlAddAttribute should be a module procedure so we can
          ! handle all types and dimensions in a useful way.
          select case(xtype)
          case (NF90_CHAR)
            call check ( nf90_get_att(ncid, i, atname, str) )
            call ncmlAddAttribute( xf, trim(atname), xtype, str(1:len) )
          case default
            if (len /= 1) print*, "Can not handle array attributes"
            print*, len
            print*, "Can not handle this attribute type"
            call ncmlAddAttribute( xf, trim(atname), xtype )
          end select
      enddo 
    endif
!
! Handle each variable in turn
! Variable may contain attributes
!
    do i = 1, nvar
      call check( NF90_INQUIRE_VARIABLE(ncid, i, name, xtype, ndims=ndims) )
      call check( NF90_INQUIRE_VARIABLE(ncid, i, name, xtype, dimids=DimIds(:ndims),  nAtts=natts) )
      call ncmlStartVariable( xf, name, xtype, trim(namedims(ncid, ndims, DimIds(:ndims))) )

      if ( natts > 0 ) then
        do j = 1, natts
          call check( NF90_INQ_ATTNAME(ncid, i, j, atname) )
          call check( NF90_INQUIRE_ATTRIBUTE(ncid, i, trim(atname), xtype, len) )
          ! FIXME: ncmlAddAttribute should be a module procedure so we can
          ! handle all types and dimensions in a useful way.
          select case(xtype)
          case (NF90_CHAR)
            call check ( nf90_get_att(ncid, i, atname, str) )
            call ncmlAddAttribute( xf, trim(atname), xtype, str(1:len) )
          case default
            if (len /= 1) print*, "Can not handle array attributes"
            print*, len
            print*, "Can not handle this attribute type"
            call ncmlAddAttribute( xf, trim(atname), xtype )
          end select
        enddo
      endif

      call ncmlEndVariable( xf )
    enddo

  call ncmlEndContainer( xf )

  end subroutine ncmlDumpContainer

end module wncml
