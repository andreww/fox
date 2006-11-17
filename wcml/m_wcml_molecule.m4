dnl
include(`foreach.m4')dnl
dnl
include(`common.m4')dnl
dnl
dnl Below we only use arguments with a type of xsd:string
define(`TOHWM4_moleculeargs', `(dictRef,convention,title,id,ref,formula,chirality,role)')dnl
dnl
define(`TOHWM4_moleculeargslist', `dnl
m4_foreach(`x', TOHWM4_moleculeargs, `,x')dnl
')dnl
dnl
define(`TOHWM4_moleculeargsdecl',`dnl
m4_foreach(`x',TOHWM4_moleculeargs,`TOHWM4_dummyargdecl(x)')
')dnl
define(`TOHWM4_moleculeargsuse',`dnl
m4_foreach(`x',TOHWM4_moleculeargs,`TOHWM4_dummyarguse(x)')
')dnl
define(`TOHWM4_molecule_subs', `dnl
  subroutine cmlAddMolecule$1(xf, elements, atomRefs, coords, occupancies, atomIds, style, fmt &
TOHWM4_moleculeargslist)
    type(xmlf_t), intent(inout) :: xf
    real(kind=$1), intent(in)              :: coords(:, :)
    character(len=*), intent(in)           :: elements(:)
    character(len=*), intent(in), optional :: atomRefs(:) 
    real(kind=$1), intent(in), optional :: occupancies(:)
    character(len=*), intent(in), optional :: atomIds(:)
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: style

TOHWM4_moleculeargsdecl

    integer          :: i

    call xml_NewElement(xf, "molecule")

TOHWM4_moleculeargsuse

    call xml_NewElement(xf, "atomArray")

    do i = 1, size(coords,2)
      call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
           coords=coords(:, i), style=style, fmt=fmt)
      if (present(occupancies)) call xml_AddAttribute(xf, "occupancy", occupancies(i))
      if (present(atomRefs)) call xml_AddAttribute(xf, "ref", atomRefs(i))
      if (present(atomIds)) call xml_AddAttribute(xf, "id", atomIds(i))
      call xml_EndElement(xf, "atom")
     enddo

    call xml_EndElement(xf, "atomArray")
    call xml_EndElement(xf, "molecule")

  end subroutine cmlAddMolecule$1


  subroutine cmlAddMolecule$1_sh(xf, natoms, elements, atomRefs, coords, occupancies, atomIds, style, fmt &
TOHWM4_moleculeargslist)
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in) :: natoms
    real(kind=$1), intent(in)              :: coords(3, natoms)
    character(len=*), intent(in)           :: elements(natoms)
    character(len=*), intent(in), optional :: atomRefs(natoms) 
    real(kind=$1), intent(in), optional :: occupancies(natoms) 
    character(len=*), intent(in), optional :: atomIds(natoms) 
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: style

TOHWM4_moleculeargsdecl

    integer          :: i

    call xml_NewElement(xf, "molecule")

TOHWM4_moleculeargsuse

    call xml_NewElement(xf, "atomArray")


    do i = 1, natoms
      call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
           coords=coords(:, i), style=style, fmt=fmt)
      if (present(occupancies)) call xml_AddAttribute(xf, "occupancy", occupancies(i))
      if (present(atomRefs)) call xml_AddAttribute(xf, "ref", atomRefs(i))
      if (present(atomIds)) call xml_AddAttribute(xf, "id", atomIds(i))
      call xml_EndElement(xf, "atom")
     enddo

    call xml_EndElement(xf, "atomArray")
    call xml_EndElement(xf, "molecule")
    
  end subroutine cmlAddMolecule$1_sh


  subroutine cmlAddMolecule_3_$1(xf, elements, x, y, z, atomRefs, occupancies, atomIds, style, fmt &
TOHWM4_moleculeargslist)
    type(xmlf_t), intent(inout) :: xf
    real(kind=$1), intent(in)              :: x(:)
    real(kind=$1), intent(in)              :: y(:)
    real(kind=$1), intent(in)              :: z(:)
    character(len=*), intent(in)           :: elements(:)
    character(len=*), intent(in), optional :: atomRefs(:) 
    character(len=*), intent(in), optional :: atomIds(:) 
    real(kind=$1), intent(in), optional :: occupancies(:) 
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: style

TOHWM4_moleculeargsdecl

    integer          :: i

    call xml_NewElement(xf, "molecule")

TOHWM4_moleculeargsuse

    call xml_NewElement(xf, "atomArray")

    do i = 1, size(x)
      call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
           coords=(/x(i), y(i), z(i)/), style=style, fmt=fmt)
      if (present(occupancies)) call xml_AddAttribute(xf, "occupancy", occupancies(i))
      if (present(atomRefs)) call xml_AddAttribute(xf, "ref", atomRefs(i))
      if (present(atomIds)) call xml_AddAttribute(xf, "id", atomIDs(i))
      call xml_EndElement(xf, "atom")
     enddo

    call xml_EndElement(xf, "atomArray")
    call xml_EndElement(xf, "molecule")
    
  end subroutine cmlAddMolecule_3_$1


  subroutine cmlAddMolecule_3_$1_sh(xf, natoms, elements, x, y, z, atomRefs, occupancies, atomIds, style, fmt &
TOHWM4_moleculeargslist)
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in) :: natoms
    real(kind=$1), intent(in)              :: x(natoms)
    real(kind=$1), intent(in)              :: y(natoms)
    real(kind=$1), intent(in)              :: z(natoms)
    character(len=*), intent(in)           :: elements(natoms)
    character(len=*), intent(in), optional :: atomRefs(natoms)
    character(len=*), intent(in), optional :: atomIds(natoms)
    real(kind=$1), intent(in), optional :: occupancies(natoms)
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: style

TOHWM4_moleculeargsdecl

    integer          :: i

    call xml_NewElement(xf, "molecule")

TOHWM4_moleculeargsuse

    call xml_NewElement(xf, "atomArray")

    do i = 1, natoms
      call cmlAddAtom(xf=xf, elem=trim(elements(i)), &
           coords=(/x(i), y(i), z(i)/), style=style, fmt=fmt)
      if (present(occupancies)) call xml_AddAttribute(xf, "occupancy", occupancies(i))
      if (present(atomRefs)) call xml_AddAttribute(xf, "ref", atomRefs(i))
      if (present(atomIds)) call xml_AddAttribute(xf, "id", atomIds(i))
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
      case default
        call FoX_error("Invalid style specification for atomic coordinates")
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
  use m_common_error, only: FoX_error
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
