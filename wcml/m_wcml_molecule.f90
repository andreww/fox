module m_wcml_molecule

  use m_common_error, only: FoX_error
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_EndElement
  use FoX_wxml, only: xml_AddAttribute

  implicit none
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)

  interface cmlAddMolecule
!    module procedure cmlAddMoleculeSP
    module procedure cmlAddMoleculeDP
!    module procedure cmlAddMolecule3SP
!    module procedure cmlAddMolecule3DP
  end interface

  public :: cmlAddMolecule

contains

  subroutine cmlAddMoleculeDP(xf, natoms, elements, refs, coords, occupancies, style, id, title, dictref, fmt)

    type(xmlf_t), intent(inout) :: xf
    integer, intent(in)                    :: natoms             ! number of atoms
    real(kind=dp), intent(in)              :: coords(:, :)  ! atomic coordinates
    character(len=*), intent(in)           :: elements(natoms)   ! chemical element types
    character(len=*), intent(in), optional :: refs(natoms) 
    real(kind=dp), intent(in), optional :: occupancies(natoms) 
    character(len=*), intent(in), optional :: id                 ! id
    character(len=*), intent(in), optional :: title              ! the title
    character(len=*), intent(in), optional :: dictref            ! the dictionary reference
    character(len=*), intent(in), optional :: fmt                ! format for coords
    character(len=*), intent(in), optional :: style              ! type of coordinates 

    integer          :: i

    call xml_NewElement(xf, 'molecule')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'id', title)
    if (present(dictRef)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    call xml_NewElement(xf, 'atomArray')

    do i = 1, natoms
      if (present(occupancies)) then
        call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
          occupancyDP=occupancies(i))
      else
        call cmlAddAtom(xf=xf, elem=trim(elements(i)))
      endif
      if (present(refs)) call xml_AddAttribute(xf, 'ref', refs(i))
      if (present(style)) then
        select case(style)
        case ('x3') 
          call addcoords_x3_dp(xf, coords(:, i), fmt)
        case ('xFrac')
          call addcoords_xfrac_dp(xf, coords(:, i), fmt)
        case ('xyz3')
          call addcoords_xyz3_dp(xf, coords(:, i), fmt)
        case ('xyzFrac')
          call addcoords_xyzfrac_dp(xf, coords(:, i), fmt)
        end select
      else
        call addcoords_x3_dp(xf, coords(:, i), fmt)
      endif
      call xml_EndElement(xf, 'atom')
     enddo

    call xml_EndElement(xf, 'atomArray')
    call xml_EndElement(xf, 'molecule')
    
  end subroutine cmlAddMoleculeDP

!!$  
!!$  ! -------------------------------------------------
!!$  ! 2. writes complete SP molecule to xml channel
!!$  ! -------------------------------------------------
!!$  
!!$  subroutine cmlAddMoleculeSP(xf, natoms, elements, refs, coords, occupancies, style, id, title, dictref, fmt)
!!$    type(xmlf_t), intent(inout) :: xf
!!$    integer, intent(in)                    :: natoms          ! number of atoms
!!$    character(len=*), intent(in)           :: elements(*)     ! chemical element types
!!$    real(kind=sp), intent(in)              :: coords(3, *)    ! atomic coordinates
!!$    character(len=*), intent(in), optional :: refs(natoms)    ! id
!!$    real(kind=sp), intent(in), optional :: occupancies(natoms) 
!!$    character(len=*), intent(in), optional :: id              ! id
!!$    character(len=*), intent(in), optional :: title           ! the title
!!$    character(len=*), intent(in), optional :: dictref         ! the dictionary reference
!!$    character(len=*), intent(in), optional :: fmt             ! format for coords
!!$    character(len=*), intent(in), optional :: style           ! type of coordinates ('x3'for Cartesians, 'xFrac' 
!!$    ! for fractionals; ' ' = default => cartesians)
!!$    ! Flush on entry and exit
!!$    !character(len=6) :: id1, id0
!!$    integer          :: i
!!$    character(len=10):: stylei
!!$
!!$    if (present(style)) then
!!$       stylei = style
!!$    else
!!$       stylei = 'x3'
!!$    endif
!!$
!!$    call xml_NewElement(xf, 'molecule')
!!$    if (present(id)) call xml_AddAttribute(xf, 'id', id)
!!$    if (present(title)) call xml_AddAttribute(xf, 'id', title)
!!$    if (present(dictRef)) call xml_AddAttribute(xf, 'dictRef', dictRef)
!!$    call xml_NewElement(xf, 'atomArray')
!!$    do i = 1, natoms
!!$       !write(id0, '(i4)') i
!!$       !id0 = adjustl(id0)
!!$       !id1 = 'a'
!!$       !id1(2:) = id0
!!$       !call cmlAddAtom(xf=xf, elem=elements(i), id=trim(id1))
!!$      if (present(occupancies)) then
!!$        call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
!!$          occupancySP=occupancies(i))
!!$      else
!!$        call cmlAddAtom(xf=xf, elem=trim(elements(i)))
!!$      endif
!!$       call cmlAddAtom(xf=xf, elem=trim(elements(i)))
!!$       if (present(refs)) call xml_AddAttribute(xf, 'ref', refs(i))
!!$       if (stylei .eq. 'x3') then
!!$          call CMLATX39SP(xf, coords(:, i), fmt)
!!$       elseif (stylei .eq. 'xFrac') then
!!$          call CMLATXF9SP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
!!$       elseif (stylei .eq. 'xyz3') then
!!$          call CMLATXYZ39SP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
!!$       elseif (stylei .eq. 'xyzFrac') then
!!$          call CMLATXYZFRACT9SP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
!!$       endif
!!$       call xml_EndElement(xf, 'atom')
!!$    enddo
!!$
!!$    call xml_EndElement(xf, 'atomArray')
!!$    call xml_EndElement(xf, 'molecule')
!!$    
!!$    
!!$  end subroutine cmlAddMoleculeSP
!!$  
!!$  
!!$  ! -------------------------------------------------
!!$  ! 1. writes complete DP molecule to xml channel (No. 2)
!!$  ! -------------------------------------------------
!!$  
!!$  subroutine cmlAddMolecule3DP(xf, natoms, elements, x, y, z, style, id, title, dictref, fmt)
!!$    type(xmlf_t), intent(inout) :: xf
!!$    integer, intent(in)                    :: natoms          ! number of atoms
!!$    real(kind=dp), intent(in)               :: x(*)
!!$    real(kind=dp), intent(in)               :: y(*)
!!$    real(kind=dp), intent(in)               :: z(*)
!!$    character(len=*), intent(in)           :: elements(*)     ! chemical element types
!!$    character(len=*), intent(in), optional :: id              ! id
!!$    character(len=*), intent(in), optional :: title           ! the title
!!$    character(len=*), intent(in), optional :: dictref         ! the dictionary reference
!!$    character(len=*), intent(in), optional :: fmt             ! format for coords
!!$    character(len=*), intent(in), optional :: style           ! type of coordinates ('x3' for Cartesians, 'xFrac' 
!!$    ! for fractionals; ' ' = default => cartesians)
!!$    !character(len=6)  :: id1, id0
!!$    integer           :: i
!!$    character(len=10) :: stylei
!!$
!!$    if (present(style)) then
!!$       stylei = style
!!$    else
!!$       stylei = 'x3'
!!$    endif
!!$
!!$    call xml_NewElement(xf, 'molecule')
!!$    if (present(id)) call xml_AddAttribute(xf, 'id', id)
!!$    if (present(title)) call xml_AddAttribute(xf, 'id', title)
!!$    if (present(dictRef)) call xml_AddAttribute(xf, 'dictRef', dictRef)
!!$    call xml_NewElement(xf, 'atomArray')
!!$
!!$    do i = 1, natoms
!!$       !write(id0, '(i4)') i
!!$       !id0 = adjustl(id0)
!!$       !id1 = 'a'
!!$       !id1(2:) = id0
!!$       !call cmlAddAtom(xf=xf, elem=elements(i), id=trim(id1))
!!$       call cmlAddAtom(xf=xf, elem=trim(elements(i)))
!!$       if (stylei .eq. 'x3') then
!!$          call addcoords_x3_dp(xf, (/x(i), y(i), z(i)/), fmt)
!!$       elseif (stylei .eq. 'xFrac') then
!!$          call addcoords_xfrac_dp(xf, x(i), y(i), z(i), fmt)
!!$       elseif (stylei .eq. 'xyz3') then
!!$          call addcoords_xyz3_dp(xf, x(i), y(i), z(i), fmt)
!!$       elseif (stylei .eq. 'xyzFrac') then
!!$          call addcoords_xyzfrac_dp(xf, x(i), y(i), z(i), fmt)
!!$       endif
!!$       call xml_EndElement(xf, 'atom')
!!$    enddo
!!$
!!$    call xml_EndElement(xf, 'atomArray')
!!$    call xml_EndElement(xf, 'molecule')
!!$    
!!$  end subroutine cmlAddMolecule3DP
!!$  
!!$  
!!$  ! -------------------------------------------------
!!$  ! 2. writes complete SP molecule to xml channel (No. 2)
!!$  ! -------------------------------------------------
!!$  
!!$  subroutine cmlAddMolecule3SP(xf, natoms, elements, x, y, z, style, id, title, dictref, fmt)
!!$
!!$
!!$    type(xmlf_t), intent(inout) :: xf
!!$    ! 10 Arguments
!!$    integer, intent(in)                    :: natoms          ! number of atoms
!!$    real(kind=sp), intent(in)               :: x(*)
!!$    real(kind=sp), intent(in)               :: y(*)
!!$    real(kind=sp), intent(in)               :: z(*)
!!$    character(len=*), intent(in)           :: elements(*)      ! chemical element types
!!$    character(len=*), intent(in), optional :: id               ! id
!!$    character(len=*), intent(in), optional :: title            ! the title
!!$    character(len=*), intent(in), optional :: dictref          ! the dictionary reference
!!$    character(len=*), intent(in), optional :: fmt              ! format for coords
!!$    character(len=*), intent(in), optional :: style            ! type of coordinates ('x3' for Cartesians, 'xFrac' 
!!$    ! for fractionals; ' ' = default => cartesians)
!!$
!!$    integer           :: i
!!$    character(len=10) :: stylei
!!$
!!$    if (present(style)) then
!!$       stylei = style
!!$    else
!!$       stylei = 'x3'
!!$    endif
!!$
!!$    call xml_NewElement(xf, 'molecule')
!!$    call xml_AddAttribute(xf, 'id', id)
!!$    call xml_AddAttribute(xf, 'title', title)
!!$    call xml_AddAttribute(xf, 'dictref', dictref)
!!$    call xml_NewElement(xf, 'atomArray')
!!$    do i = 1, natoms
!!$       !write(id0, '(i4)') i
!!$       !id0 = adjustl(id0)
!!$       !id1 = 'a'
!!$       !id1(2:) = id0
!!$       !call cmlAddAtom(xf=xf, elem=elements(i), id=trim(id1))
!!$       call cmlAddAtom(xf=xf, elem=trim(elements(i)))
!!$       if (stylei .eq. 'x3') then
!!$          call CMLATX39SP(xf, (/x(i), y(i), z(i)/), fmt)
!!$       else if (stylei .eq. 'xFrac') then
!!$          call CMLATXF9SP(xf, x(i), y(i), z(i), fmt)
!!$       else if (stylei .eq. 'xyz3') then
!!$          call CMLATXYZ39SP(xf, x(i), y(i), z(i), fmt)
!!$       else if (stylei .eq. 'xyzFrac') then
!!$          call CMLATXYZFRACT9SP(xf, x(i), y(i), z(i), fmt)
!!$       endif
!!$           call xml_EndElement(xf, 'atom')
!!$    enddo
!!$
!!$    call xml_EndElement(xf, 'atomArray')
!!$    call xml_EndElement(xf, 'molecule')
!!$
!!$  end subroutine cmlAddMolecule3SP
  
  ! -------------------------------------------------
  ! writes an <atom> start tag
  ! -------------------------------------------------
  
  subroutine cmlAddAtom(xf, elem, id, charge, hCount, occupancySP, occupancyDP, fmt)
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in), optional           :: charge     ! formalCharge
    integer, intent(in), optional           :: hCount     ! hydrogenCount
    real(kind=sp), intent(in), optional     :: occupancySP
    real(kind=dp), intent(in), optional     :: occupancyDP
    character(len=*), intent(in), optional  :: elem       ! chemical element name
    character(len=*), intent(in), optional  :: id         ! atom id
    character(len=*), intent(in), optional  :: fmt        ! format

    call xml_NewElement(xf, 'atom')
    if (present(elem))      call xml_AddAttribute(xf, 'elementType', elem)
    if (present(id))        call xml_AddAttribute(xf, 'id', id)
    if (present(charge))    call xml_AddAttribute(xf, 'formalCharge', charge)
    if (present(hCount))    call xml_AddAttribute(xf, 'hydrogenCount', hCount)
    if (present(occupancySP) .and. present(occupancyDP)) &
      call FoX_error("Bad argumens to cmlAddAtom")
    if (present(occupancySP)) call xml_AddAttribute(xf, 'occupancy', occupancySP, fmt)
    if (present(occupancyDP)) call xml_AddAttribute(xf, 'occupancy', occupancyDP, fmt)

  end subroutine cmlAddAtom
  

  subroutine addcoords_xyz3_dp(xf, coords, fmt)
    type(xmlf_t), intent(inout)              :: xf
    real(kind=dp), intent(in), dimension(:) :: coords
    character(len=*), intent(in), optional   :: fmt        ! format

    select case (size(coords))
    case (2)
      call xml_AddAttribute(xf, 'xy2', coords,fmt)
    case(3)
      call xml_AddAttribute(xf, 'xyz3', coords,fmt)
    end select

  end subroutine addcoords_xyz3_dp

  subroutine CMLATXYZ39SP(xf, coords, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=sp), intent(in), dimension(:) :: coords
    character(len=*), intent(in), optional  :: fmt        ! format

    select case (size(coords))
    case (2)
      call xml_AddAttribute(xf, 'xy2', coords, fmt)
    case(3)
      call xml_AddAttribute(xf, 'xyz3', coords, fmt)
    end select

  end subroutine CMLATXYZ39SP


  subroutine CMLATXYZFRACT9DSP(xf, coords, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=sp), intent(in), dimension(:) :: coords
    character(len=*), intent(in), optional  :: fmt        ! format

    select case (size(coords))
    case (2)
      call xml_AddAttribute(xf, 'xyFract', coords, fmt)
    case(3)
      call xml_AddAttribute(xf, 'xyzFract', coords, fmt)
    end select

  end subroutine CMLATXYZFRACT9DSP


  subroutine addcoords_xyzfrac_dp(xf, coords, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=dp), intent(in), dimension(:) :: coords
    character(len=*), intent(in), optional  :: fmt        ! format

    select case (size(coords))
    case (2)
      call xml_AddAttribute(xf, 'xyFract', coords, fmt)
    case(3)
      call xml_AddAttribute(xf, 'xyzFract', coords, fmt)
    end select

  end subroutine addcoords_xyzfrac_dp


  subroutine CMLATX39SP(xf, coords, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in), dimension(:):: coords
    character(len=*), intent(in), optional :: fmt

    select case(size(coords))
    case(2)
      call xml_AddAttribute(xf, 'x2', coords(1), fmt)
      call xml_AddAttribute(xf, 'y2', coords(2), fmt)
    case(3)
      call xml_AddAttribute(xf, 'x3', coords(1), fmt)
      call xml_AddAttribute(xf, 'y3', coords(2), fmt)
      call xml_AddAttribute(xf, 'z3', coords(3), fmt)
    end select

  end subroutine CMLATX39SP


  subroutine addcoords_x3_dp(xf, coords, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in), dimension(:):: coords
    character(len=*), intent(in), optional :: fmt

    select case(size(coords))
    case(2)
      call xml_AddAttribute(xf, 'x2', coords(1), fmt)
      call xml_AddAttribute(xf, 'y2', coords(2), fmt)
    case(3)
      call xml_AddAttribute(xf, 'x3', coords(1), fmt)
      call xml_AddAttribute(xf, 'y3', coords(2), fmt)
      call xml_AddAttribute(xf, 'z3', coords(3), fmt)
    end select

  end subroutine addcoords_x3_dp


  subroutine CMLATXF9SP(xf, coords, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=sp), intent(in), dimension(:) :: coords
    character(len=*), intent(in), optional  :: fmt

    call xml_AddAttribute(xf, 'xFract', coords(1), fmt)
    call xml_AddAttribute(xf, 'yFract', coords(2), fmt)
    call xml_AddAttribute(xf, 'zFract', coords(3), fmt)

  end subroutine CMLATXF9SP


  subroutine addcoords_xfrac_dp(xf, coords, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=dp), intent(in), dimension(:) :: coords
    character(len=*), intent(in), optional  :: fmt

    call xml_AddAttribute(xf, 'xFract', coords(1), fmt)
    call xml_AddAttribute(xf, 'yFract', coords(2), fmt)
    call xml_AddAttribute(xf, 'zFract', coords(3), fmt)

  end subroutine addcoords_xfrac_dp

end module m_wcml_molecule
