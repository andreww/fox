module m_wcml_geometry

  use m_common_realtypes, only: sp, dp
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_EndElement
  use FoX_wxml, only: xml_AddAttribute, xml_AddCharacters

  implicit none
  private

  interface cmlAddLength
    module procedure cmlAddLength_SP, cmlAddLength_DP
  end interface

  interface cmlAddAngle
    module procedure cmlAddAngle_SP, cmlAddAngle_DP
  end interface

  interface cmlAddTorsion
    module procedure cmlAddTorsion_SP, cmlAddTorsion_DP
  end interface

  public :: cmlAddLength
  public :: cmlAddAngle
  public :: cmlAddTorsion

contains

  subroutine cmlAddLength_SP(xf, id, atomRef1, atomRef2, length, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id           ! length id
    character(len=*), intent(in) :: atomRef1     ! ref to first atom
    character(len=*), intent(in) :: atomRef2     ! ref to second atom
    real(kind=sp), intent(in)    :: length       ! the length
    character(len=*), intent(in), optional :: fmt          ! format

    call xml_NewElement(xf, 'length')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs2', atomRef1//' '//atomRef2)
    call xml_AddCharacters(xf, length, fmt)
    call xml_EndElement(xf, 'length')

  end subroutine cmlAddLength_SP
  

  subroutine cmlAddLength_DP(xf, id, atomRef1, atomRef2, length, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id           ! length id
    character(len=*), intent(in) :: atomRef1     ! ref to first atom
    character(len=*), intent(in) :: atomRef2     ! ref to second atom
    real(kind=dp), intent(in)    :: length       ! the length
    character(len=*), intent(in), optional :: fmt          ! format

    call xml_NewElement(xf, 'length')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs2', atomRef1//' '//atomRef2)
    call xml_AddCharacters(xf, length, fmt)
    call xml_EndElement(xf, 'length')

  end subroutine cmlAddLength_DP
  

  subroutine cmlAddAngle_SP(xf, id, atomRef1, atomRef2, atomRef3, angle, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id              ! angle id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    real(kind=sp), intent(in)     :: angle          ! the angle
    character(len=*), intent(in), optional :: fmt             ! format

    call xml_NewElement(xf, 'angle')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs3', atomRef1//' '//atomRef2//' '//atomRef3)
    call xml_AddCharacters(xf, angle, fmt)
    call xml_EndElement(xf, 'angle')

  end subroutine cmlAddAngle_SP


  subroutine cmlAddAngle_DP(xf, id, atomRef1, atomRef2, atomRef3, angle, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id              ! angle id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    real(kind=dp), intent(in)     :: angle          ! the angle
    character(len=*), intent(in), optional :: fmt             ! format

    call xml_NewElement(xf, 'angle')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs3', atomRef1//' '//atomRef2//' '//atomRef3)
    call xml_AddCharacters(xf, angle, fmt)
    call xml_EndElement(xf, 'angle')

  end subroutine cmlAddAngle_DP


  subroutine cmlAddTorsion_SP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id              ! torsion id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    character(len=*), intent(in) :: atomRef4        ! ref to fourth atom
    real(kind=sp), intent(in)    :: torsion         ! the torsion
    character(len=*), intent(in), optional :: fmt             ! format

    call xml_NewElement(xf, 'torsion')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs4', &
         atomRef1//' '//atomRef2//' '//atomRef3//' '//atomRef4)
    call xml_AddCharacters(xf, torsion, fmt)
    call xml_EndElement(xf, 'torsion')

  end subroutine cmlAddTorsion_SP


  subroutine cmlAddTorsion_DP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id              ! torsion id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    character(len=*), intent(in) :: atomRef4        ! ref to fourth atom
    real(kind=dp), intent(in)    :: torsion         ! the torsion
    character(len=*), intent(in), optional :: fmt             ! format

    call xml_NewElement(xf, 'torsion')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs4', &
         atomRef1//' '//atomRef2//' '//atomRef3//' '//atomRef4)
    call xml_AddCharacters(xf, torsion, fmt)
    call xml_EndElement(xf, 'torsion')

  end subroutine cmlAddTorsion_DP

end module m_wcml_geometry
