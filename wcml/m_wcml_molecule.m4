define(`TOHWM4_molecule_subs', `dnl
  subroutine cmlAddMolecule$1(xf, elements, refs, coords, occupancies, style, id, title, dictref, fmt)
    type(xmlf_t), intent(inout) :: xf
    real(kind=$1), intent(in)              :: coords(:, :)
    character(len=*), intent(in)           :: elements(:)
    character(len=*), intent(in), optional :: refs(:) 
    real(kind=$1), intent(in), optional :: occupancies(:) 
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: style

    integer          :: i

    call xml_NewElement(xf, "molecule")
    if (present(id)) call xml_AddAttribute(xf, "id", id)
    if (present(title)) call xml_AddAttribute(xf, "title", title)
    if (present(dictRef)) call xml_AddAttribute(xf, "dictRef", dictRef)
    call xml_NewElement(xf, "atomArray")

    do i = 1, size(coords,2)
      call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
           coords=coords(:, i), style=style, fmt=fmt)
      if (present(occupancies)) call xml_AddAttribute(xf, "occupancy", occupancies(i))
      if (present(refs)) call xml_AddAttribute(xf, "ref", refs(i))
      call xml_EndElement(xf, "atom")
     enddo

    call xml_EndElement(xf, "atomArray")
    call xml_EndElement(xf, "molecule")

  end subroutine cmlAddMolecule$1


  subroutine cmlAddMolecule$1_sh(xf, natoms, elements, refs, coords, occupancies, style, id, title, dictref, fmt)
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in) :: natoms
    real(kind=$1), intent(in)              :: coords(3, natoms)
    character(len=*), intent(in)           :: elements(natoms)
    character(len=*), intent(in), optional :: refs(natoms) 
    real(kind=$1), intent(in), optional :: occupancies(natoms) 
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: style

    integer          :: i

    call xml_NewElement(xf, "molecule")
    if (present(id)) call xml_AddAttribute(xf, "id", id)
    if (present(title)) call xml_AddAttribute(xf, "title", title)
    if (present(dictRef)) call xml_AddAttribute(xf, "dictRef", dictRef)
    call xml_NewElement(xf, "atomArray")


    do i = 1, natoms
      call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
           coords=coords(:, i), style=style, fmt=fmt)
      if (present(occupancies)) call xml_AddAttribute(xf, "occupancy", occupancies(i))
      if (present(refs)) call xml_AddAttribute(xf, "ref", refs(i))
      call xml_EndElement(xf, "atom")
     enddo

    call xml_EndElement(xf, "atomArray")
    call xml_EndElement(xf, "molecule")
    
  end subroutine cmlAddMolecule$1_sh


  subroutine cmlAddMolecule_3_$1(xf, elements, x, y, z, refs, occupancies, style, id, title, dictref, fmt)
    type(xmlf_t), intent(inout) :: xf
    real(kind=$1), intent(in)              :: x(:)
    real(kind=$1), intent(in)              :: y(:)
    real(kind=$1), intent(in)              :: z(:)
    character(len=*), intent(in)           :: elements(:)
    character(len=*), intent(in), optional :: refs(:) 
    real(kind=$1), intent(in), optional :: occupancies(:) 
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: style

    integer          :: i

    call xml_NewElement(xf, "molecule")
    if (present(id)) call xml_AddAttribute(xf, "id", id)
    if (present(title)) call xml_AddAttribute(xf, "title", title)
    if (present(dictRef)) call xml_AddAttribute(xf, "dictRef", dictRef)
    call xml_NewElement(xf, "atomArray")

    do i = 1, size(x)
      call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
           coords=(/x(i), y(i), z(i)/), style=style, fmt=fmt)
      if (present(occupancies)) call xml_AddAttribute(xf, "occupancy", occupancies(i))
      if (present(refs)) call xml_AddAttribute(xf, "ref", refs(i))
      call xml_EndElement(xf, "atom")
     enddo

    call xml_EndElement(xf, "atomArray")
    call xml_EndElement(xf, "molecule")
    
  end subroutine cmlAddMolecule_3_$1


  subroutine cmlAddMolecule_3_$1_sh(xf, natoms, elements, x, y, z, refs, occupancies, style, id, title, dictref, fmt)
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in) :: natoms
    real(kind=$1), intent(in)              :: x(natoms)
    real(kind=$1), intent(in)              :: y(natoms)
    real(kind=$1), intent(in)              :: z(natoms)
    character(len=*), intent(in)           :: elements(natoms)
    character(len=*), intent(in), optional :: refs(natoms)
    real(kind=$1), intent(in), optional :: occupancies(natoms)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: style

    integer          :: i

    call xml_NewElement(xf, "molecule")
    if (present(id)) call xml_AddAttribute(xf, "id", id)
    if (present(title)) call xml_AddAttribute(xf, "title", title)
    if (present(dictRef)) call xml_AddAttribute(xf, "dictRef", dictRef)
    call xml_NewElement(xf, "atomArray")

    do i = 1, natoms
      call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
           coords=(/x(i), y(i), z(i)/), style=style, fmt=fmt)
      if (present(occupancies)) call xml_AddAttribute(xf, "occupancy", occupancies(i))
      if (present(refs)) call xml_AddAttribute(xf, "ref", refs(i))
      call xml_EndElement(xf, "atom")
     enddo

    call xml_EndElement(xf, "atomArray")
    call xml_EndElement(xf, "molecule")
    
  end subroutine cmlAddMolecule_3_$1_sh

  subroutine cmlAddAtom_$1(xf, elem, coords, id, charge, hCount, occupancy, &
       fmt, style)
    type(xmlf_t), intent(inout) :: xf
    real(kind=$1), intent(in), dimension(:) :: coords
    character(len=*), intent(in) :: elem
    integer, intent(in), optional           :: charge
    integer, intent(in), optional           :: hCount
    real(kind=$1), intent(in), optional     :: occupancy
    character(len=*), intent(in), optional  :: id
    character(len=*), intent(in), optional  :: fmt
    character(len=*), intent(in), optional  :: style
    

    call xml_NewElement(xf, "atom")
    call xml_AddAttribute(xf, "elementType", elem)
    if (present(id))        call xml_AddAttribute(xf, "id", id)
    if (present(charge))    call xml_AddAttribute(xf, "formalCharge", charge)
    if (present(hCount))    call xml_AddAttribute(xf, "hydrogenCount", hCount)
    if (present(occupancy)) call xml_AddAttribute(xf, "occupancy", occupancy, fmt)

    if (present(style)) then
      select case(style)
      case ("x3") 
        call addcoords_x3_$1(xf, coords, fmt)
      case ("xFrac")
        call addcoords_xfrac_$1(xf, coords, fmt)
      case ("xyz3")
        call addcoords_xyz3_$1(xf, coords, fmt)
      case ("xyzFrac")
        call addcoords_xyzfrac_$1(xf, coords, fmt)
      end select
    else
      call addcoords_x3_$1(xf, coords, fmt)
    endif

  end subroutine cmlAddAtom_$1


  subroutine addcoords_xyz3_$1(xf, coords, fmt)
    type(xmlf_t), intent(inout)              :: xf
    real(kind=$1), intent(in), dimension(:) :: coords
    character(len=*), intent(in), optional   :: fmt

    select case (size(coords))
    case (2)
      call xml_AddAttribute(xf, "xy2", coords,fmt)
    case(3)
      call xml_AddAttribute(xf, "xyz3", coords,fmt)
    end select

  end subroutine addcoords_xyz3_$1


  subroutine addcoords_xyzfrac_$1(xf, coords, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=$1), intent(in), dimension(:) :: coords
    character(len=*), intent(in), optional  :: fmt

    select case (size(coords))
    case (2)
      call xml_AddAttribute(xf, "xyFract", coords, fmt)
    case(3)
      call xml_AddAttribute(xf, "xyzFract", coords, fmt)
    end select

  end subroutine addcoords_xyzfrac_$1


  subroutine addcoords_x3_$1(xf, coords, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=$1), intent(in), dimension(:):: coords
    character(len=*), intent(in), optional :: fmt

    select case(size(coords))
    case(2)
      call xml_AddAttribute(xf, "x2", coords(1), fmt)
      call xml_AddAttribute(xf, "y2", coords(2), fmt)
    case(3)
      call xml_AddAttribute(xf, "x3", coords(1), fmt)
      call xml_AddAttribute(xf, "y3", coords(2), fmt)
      call xml_AddAttribute(xf, "z3", coords(3), fmt)
    end select

  end subroutine addcoords_x3_$1


  subroutine addcoords_xfrac_$1(xf, coords, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=$1), intent(in), dimension(:) :: coords
    character(len=*), intent(in), optional  :: fmt

    call xml_AddAttribute(xf, "xFract", coords(1), fmt)
    call xml_AddAttribute(xf, "yFract", coords(2), fmt)
    call xml_AddAttribute(xf, "zFract", coords(3), fmt)

  end subroutine addcoords_xfrac_$1
')dnl
dnl
! This file is AUTOGENERATED
! To change, edit m_wcml_molecule.m4 and regenerate

module m_wcml_molecule

  use m_common_realtypes, only: sp, dp
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_EndElement
  use FoX_wxml, only: xml_AddAttribute

  implicit none
  private

  interface cmlAddMolecule
    module procedure cmlAddMoleculeSP
    module procedure cmlAddMoleculeSP_sh
    module procedure cmlAddMolecule_3_SP
    module procedure cmlAddMolecule_3_SP_sh
    module procedure cmlAddMoleculeDP
    module procedure cmlAddMoleculeDP_sh
    module procedure cmlAddMolecule_3_DP
    module procedure cmlAddMolecule_3_DP_sh
  end interface

  interface cmlAddAtom
    module procedure cmlAddAtom_sp
    module procedure cmlAddAtom_dp
  end interface

  public :: cmlAddMolecule

contains


TOHWM4_molecule_subs(`sp')

TOHWM4_molecule_subs(`dp')

end module m_wcml_molecule
