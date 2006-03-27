module m_wcml_core

  use xmlf90_wxml, only: xmlf90_version, xmlf_t, str
  use xmlf90_wxml, only: xml_NewElement, xml_AddPcData, xml_AddAttribute
  use xmlf90_wxml, only: xml_EndElement
  use m_wcml_stml, only: stmAddScalar
  use m_wcml_stml, only: stmAddMatrix
  use m_wcml_stml, only: stmAddArray
  use m_wcml_stml, only: stmAddStartTag
  
!  use m_wcml_coma

  PRIVATE

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)
  character(len=*), parameter :: spformat = '(es11.3)'
  character(len=*), parameter :: dpformat = '(es16.8)'
  
! CMLUnits
  character(len=*), parameter :: U_ANGSTR = 'units:angstrom'
  character(len=*), parameter :: U_PMETER = 'units:pm'
  character(len=*), parameter :: U_DEGREE = 'units:degree'
  character(len=*), parameter :: U_RADIAN = 'units:radian'
  character(len=*), parameter :: U_INVCM  = 'units:cm-1'
  character(len=*), parameter :: U_KCALMO = 'units:kcal-mole'
  character(len=*), parameter :: U_EVOLT  = 'units:ev'
  character(len=*), parameter :: U_SECOND = 'units:second'
  character(len=*), parameter :: U_VOLT   = 'units:volt'

  public :: cmlStartCml
  public :: cmlEndCml

! CMLCore
  PUBLIC :: cmlAddCoordinates
  PUBLIC :: cmlAddLattice
  PUBLIC :: cmlAddCrystal
  PUBLIC :: cmlAddAngle
  PUBLIC :: cmlAddLength
  PUBLIC :: cmlAddEigenvalue
  PUBLIC :: cmlAddMolecule
  PUBLIC :: cmlAddMetadata
  PUBLIC :: cmlAddProperty
  PUBLIC :: cmlStArtMetadataList
  PUBLIC :: cmlEndMetadataList
  PUBLIC :: cmlStartModule
  PUBLIC :: cmlEndModule
  PUBLIC :: cmlStartParameterList
  PUBLIC :: cmlEndParameterList
  PUBLIC :: cmlStartPropertyList
  PUBLIC :: cmlEndPropertyList

! CMLComp
  PUBLIC :: cmlAddParameter
  PUBLIC :: cmlStartStep
  PUBLIC :: cmlEndStep

! CMLComa
  PUBLIC :: cmlStartBandList
  PUBLIC :: cmlEndBandList
  PUBLIC :: cmlAddBand

! CMLCore
  INTERFACE cmlAddCoordinates
     MODULE PROCEDURE cmlAddCoordinatesSP
     MODULE PROCEDURE cmlAddCoordinatesDP
  END INTERFACE

  INTERFACE cmlAddLattice
     MODULE PROCEDURE cmlAddLatticeSP
     MODULE PROCEDURE cmlAddLatticeDP
  END INTERFACE

  INTERFACE cmlAddCrystal
     MODULE PROCEDURE cmlAddCrystalSP
     MODULE PROCEDURE cmlAddCrystalDP
  END INTERFACE

  INTERFACE cmlAddAngle
     MODULE PROCEDURE cmlAddAngleSP
     MODULE PROCEDURE cmlAddAngleDP
  END INTERFACE

  INTERFACE cmlAddLength
     MODULE PROCEDURE cmlAddLengthSP
     MODULE PROCEDURE cmlAddLengthDP
  END INTERFACE

  INTERFACE cmlAddEigenvalue
     MODULE PROCEDURE cmlAddEigenvalueSP
     MODULE PROCEDURE cmlAddEigenvalueDP
  END INTERFACE

  INTERFACE cmlAddMolecule
     MODULE PROCEDURE cmlAddMoleculeSP
     MODULE PROCEDURE cmlAddMoleculeDP
     MODULE PROCEDURE cmlAddMolecule3SP
     MODULE PROCEDURE cmlAddMolecule3DP
  END INTERFACE

  INTERFACE cmlAddProperty
     MODULE PROCEDURE cmlAddPropScalarDP
     MODULE PROCEDURE cmlAddPropScalarSP
     MODULE PROCEDURE cmlAddPropScalarI
     MODULE PROCEDURE cmlAddPropScalarCh
     MODULE PROCEDURE cmlAddPropScalarLg
     MODULE PROCEDURE cmlAddPropArrayDPSi
     MODULE PROCEDURE cmlAddPropArrayDPSh
     MODULE PROCEDURE cmlAddPropArraySPSi
     MODULE PROCEDURE cmlAddPropArraySPSh
     MODULE PROCEDURE cmlAddPropArrayISi
     MODULE PROCEDURE cmlAddPropArrayISh
     MODULE PROCEDURE cmlAddPropArrayChSi
     MODULE PROCEDURE cmlAddPropArrayChSh
     MODULE PROCEDURE cmlAddPropMatrixDPSi
     MODULE PROCEDURE cmlAddPropMatrixDPSh
     MODULE PROCEDURE cmlAddPropMatrixSPSi
     MODULE PROCEDURE cmlAddPropMatrixSPSh
     MODULE PROCEDURE cmlAddPropMatrixISi
     MODULE PROCEDURE cmlAddPropMatrixISh
     MODULE PROCEDURE cmlAddPropMatrixChSi
     MODULE PROCEDURE cmlAddPropMatrixChSh
  END INTERFACE

  INTERFACE cmlAddParameter
     MODULE PROCEDURE cmlAddParameterCH
     MODULE PROCEDURE cmlAddParameterI
     MODULE PROCEDURE cmlAddParameterSP
     MODULE PROCEDURE cmlAddParameterDP
     MODULE PROCEDURE cmlAddParameterLG
  END INTERFACE

  INTERFACE cmlAddMetadata
     MODULE PROCEDURE cmlAddMetaDataCh
     MODULE PROCEDURE cmlAddMetaDataI
     MODULE PROCEDURE cmlAddMetaDataLg
  END INTERFACE

CONTAINS

  ! =================================================
  ! convenience CML routines
  ! =================================================

! Open and close a CML document.

  subroutine cmlStartCml(xf, id, title, conv, dictref, ref)
    type(xmlf_t), intent(inout) :: xf

    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref

    call xml_NewElement(xf, 'cml')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)

    call xml_AddAttribute(xf, 'xmlns', 'http://www.xml-cml.org/schema')
    call xml_AddAttribute(xf, 'xmlns:xsd', 'http://www.w3.org/2001/XMLSchema')
    call xml_AddAttribute(xf, 'xmlns:dc', 'http://purl.org/dc/elements/1.1/title')
! FIXME TOHW we will want other namespaces in here - particularly for units
! once PMR has stabilized that.

  end subroutine cmlStartCml

  subroutine cmlEndCml(xf)
    type(xmlf_t), intent(inout) :: xf

    call cmlAddMetadata(xf, name='dc:contributor', content='xmlf90-'//xmlf90_version//' (http://www.eminerals.org)')
    call xml_EndElement(xf, 'cml')

  end subroutine cmlEndCml

  ! -------------------------------------------------
  ! writes a metadataList start/end Tag to xml channel
  ! -------------------------------------------------
  SUBROUTINE cmlStartMetadataList(xf, id, title, conv, dictref, ref, role)

    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role
    
    call xml_NewElement(xf, 'metadataList')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    
  END SUBROUTINE cmlStartMetadataList

  SUBROUTINE cmlEndMetadataList(xf)

    type(xmlf_t), intent(inout) :: xf

    call xml_EndElement(xf, 'metadataList')
    
  END SUBROUTINE cmlEndMetadataList

  ! -------------------------------------------------
  ! writes a Module start/end Tag to xml channel
  ! -------------------------------------------------
  SUBROUTINE cmlStartModule(xf, id, title, conv, dictref, ref, role, serial)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role
    character(len=*), intent(in), optional :: serial
    
    call xml_NewElement(xf, 'module')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    if (present(serial)) call xml_AddAttribute(xf, 'serial', serial)
    
  END SUBROUTINE cmlStartModule

  SUBROUTINE cmlEndModule(xf)

    implicit none
    type(xmlf_t), intent(inout) :: xf

    Call xml_EndElement(xf, 'module')
    
  END SUBROUTINE cmlEndModule


  ! -------------------------------------------------
  ! writes a propertyList start/end Tag to xml channel
  ! -------------------------------------------------
  SUBROUTINE cmlStartPropertyList(xf, id, title, conv, dictref, ref, role)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role
    
    call xml_NewElement(xf, 'propertyList')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    
  END SUBROUTINE cmlStartPropertyList

  SUBROUTINE cmlEndPropertyList(xf)

    implicit none
    type(xmlf_t), intent(inout) :: xf

    Call xml_EndElement(xf, 'propertyList')
    
  END SUBROUTINE cmlEndPropertyList


  ! -------------------------------------------------
  ! writes a parameterList start/end Tag to xml channel
  ! -------------------------------------------------
  SUBROUTINE cmlStartParameterList(xf, id, title, conv, dictref, ref, role)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role
    
    call xml_NewElement(xf, 'parameterList')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    
  END SUBROUTINE cmlStartParameterList

  SUBROUTINE cmlEndParameterList(xf)

    implicit none
    type(xmlf_t), intent(inout) :: xf

    Call xml_EndElement(xf, 'parameterList')
    
  END SUBROUTINE cmlEndParameterList

  ! -------------------------------------------------
  ! writes a step start Tag to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlStartStep(xf, type, index, id, title, conv, ref)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: type
    character(len=*), intent(in), optional :: id
    integer, intent(in), optional :: index
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref

    if (present(index)) then
      call cmlStartModule(xf=xf, id=id, title=title, conv=conv, dictRef=type, ref=ref, role='step', serial=str(index))
    else
      call cmlStartModule(xf=xf, id=id, title=title, conv=conv, dictRef=type, ref=ref, role='step')
    endif
    
  END SUBROUTINE cmlStartStep

  SUBROUTINE cmlEndStep(xf)

    implicit none
    type(xmlf_t), intent(inout) :: xf

    Call xml_EndElement(xf, 'module')
    
  END SUBROUTINE cmlEndStep


  
 
  ! -------------------------------------------------
  ! 1. writes a DP property to xml channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddPropScalarDP(xf, property, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), intent(in)               :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))    call xml_AddAttribute(xf, 'ref', ref)
    call stmAddScalar(xf=xf, value=property, datatype='xsd:double', units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')

  END SUBROUTINE cmlAddPropScalarDP

  ! -------------------------------------------------
  ! 2. writes a Scalar SP property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropScalarSP(xf, property, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t),     intent(inout)        :: xf
    real(kind=sp),    intent(in)           :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddScalar(xf=xf, value=property, datatype='xsd:float', units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropScalarSP
  
  ! -------------------------------------------------
  ! 3. writes a Scalar integer property to xml channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddPropScalarI(xf, property, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in) :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddScalar(xf=xf, value=property, datatype='xsd:integer', units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropScalarI

  ! -------------------------------------------------
  ! 4. writes a DP Float matrix property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropMatrixDPSi(xf, property, nrows, ncols, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: nrows
    integer, intent(in)                    :: ncols
    real(kind=dp), Intent(in)              :: property(ncols, nrows)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf, matrix=property, ncols=ncols, nrows=nrows, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixDPSi

  SUBROUTINE cmlAddPropMatrixDPSh(xf, property, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: property(:,:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units
    
    integer :: nrows, ncols

    ncols=size(property, 1)
    nrows=size(property, 2)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf, matrix=property, ncols=ncols, nrows=nrows, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixDPSh

  ! -------------------------------------------------
  ! 5. writes an SP Float matrix property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropMatrixSPSi(xf, property, nrows, ncols, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: nrows
    integer, intent(in)                    :: ncols
    real(kind=sp), intent(in)              :: property(ncols,nrows)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf,matrix=property, ncols=ncols, nrows=nrows, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixSPSi

  SUBROUTINE cmlAddPropMatrixSPSh(xf, property, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: property(:,:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    integer :: nrows, ncols

    ncols=size(property, 1)
    nrows=size(property, 2)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf,matrix=property, ncols=ncols, nrows=nrows, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixSPSh

  ! -------------------------------------------------
  ! 6. writes an Integer matrix property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropMatrixISi(xf, property, nrows, ncols, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: nrows
    integer, intent(in)                    :: ncols
    integer, intent(in)                    :: property(nrows,ncols)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(conv))    call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf, matrix=property, ncols=ncols, nrows=nrows, units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixISi

  SUBROUTINE cmlAddPropMatrixISh(xf, property, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: property(:,:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    integer :: nrows, ncols

    ncols=size(property, 2)
    nrows=size(property, 1)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(conv))    call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf, matrix=property, ncols=ncols, nrows=nrows, units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixISh


  ! -------------------------------------------------
  ! 7. writes an Array DP property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropArrayDPSi(xf, property, nvalue, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: property(*)
    integer, intent(in)                    :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf=xf, array=property, nvalue=nvalue, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArrayDPSi

  SUBROUTINE cmlAddPropArrayDPSh(xf, property, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: property(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    integer :: nvalue
    
    nvalue=size(property)

    call xml_NewElement(xf, 'property')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf=xf, array=property, nvalue=nvalue, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArrayDPSh

  ! -------------------------------------------------
  ! 8. writes an Array SP property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropArraySPSi(xf, property, nvalue, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: property(*)
    integer, intent(in)                    :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf=xf, array=property, nvalue=nvalue, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArraySPSi

  SUBROUTINE cmlAddPropArraySPSh(xf, property, id, title, conv, dictref, ref, units, fmt)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)              :: property(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: fmt
    character(len=*), intent(in), optional :: units

    integer :: nvalue
    
    nvalue=size(property)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf=xf, array=property, nvalue=nvalue, units=units, fmt=fmt)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArraySPSh

  ! -------------------------------------------------
  ! 9. writes an Array integer property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropArrayISi(xf, property, nvalue, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: property(*)
    integer, intent(in)                    :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf, array=property, nvalue=nvalue, units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArrayISi

  SUBROUTINE cmlAddPropArrayISh(xf, property, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: property(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    integer :: nvalue

    nvalue=size(property)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf, array=property, nvalue=nvalue, units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArrayISh


  ! -------------------------------------------------
  ! 10. writes a character property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropScalarCH(xf, property, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in)           :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddScalar(xf=xf, value=property, units=units)
    call xml_EndElement(xf, 'property')

  END SUBROUTINE cmlAddPropScalarCH


  ! -------------------------------------------------
  ! 11. writes an character matrix property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropMatrixCHSi(xf, property, nrows, ncols, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t),     intent(inout)        :: xf
    integer,          intent(in)           :: nrows
    integer,          intent(in)           :: ncols
    character(len=*), intent(in)           :: property(ncols,nrows)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf, matrix=property, ncols=ncols, nrows=nrows, units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixCHSi


  SUBROUTINE cmlAddPropMatrixCHSh(xf, property, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t),     intent(inout)        :: xf
    character(len=*), intent(in)           :: property(:,:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    integer :: nrows, ncols

    ncols=size(property, 1)
    nrows=size(property, 2)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddMatrix(xf=xf, matrix=property, ncols=ncols, nrows=nrows, units=units)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropMatrixCHSh

  ! -------------------------------------------------
  ! 12. writes an character array property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropArrayCHSi(xf, property, nvalue, id, title, conv, dictref, ref)

    implicit none
    type(xmlf_t),     intent(inout)        :: xf
    character(len=*), intent(in)           :: property(*)
    integer,          intent(in)           :: nvalue
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf, array=property, nvalue=nvalue)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArrayCHSi

  SUBROUTINE cmlAddPropArrayCHSh(xf, property, id, title, conv, dictref, ref)

    implicit none
    type(xmlf_t),     intent(inout)        :: xf
    character(len=*), intent(in)           :: property(:)
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref

    integer :: nvalue

    nvalue=size(property)

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddArray(xf, array=property, nvalue=nvalue)
    call xml_EndElement(xf, 'property')
  END SUBROUTINE cmlAddPropArrayCHSh


  ! -------------------------------------------------
  ! 13. writes a logical property to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddPropScalarLG(xf, property, id, title, conv, dictref, ref, units)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    logical,          intent(in)           :: property
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: units

    call xml_NewElement(xf, 'property')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    call stmAddScalar(xf=xf, value=property, units=units)
    call xml_EndElement(xf, 'property')

  END SUBROUTINE cmlAddPropScalarLG

  !------------------------------------------------------------
  ! END OF PROPERTIES 
  !------------------------------------------------------------



  ! -------------------------------------------------
  ! 1. writes complete DP molecule to xml channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddMoleculeDP(xf, natoms, elements, refs, coords, style, id, title, dictref, fmt)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in)                    :: natoms             ! number of atoms
    real(kind=dp), intent(in)              :: coords(3, natoms)  ! atomic coordinates
    character(len=*), intent(in)           :: elements(natoms)   ! chemical element types
    character(len=*), intent(in), optional :: refs(natoms)       ! id
    character(len=*), intent(in), optional :: id                 ! id
    character(len=*), intent(in), optional :: title              ! the title
    character(len=*), intent(in), optional :: dictref            ! the dictionary reference
    character(len=*), intent(in), optional :: fmt                ! format for coords
    character(len=*), intent(in), optional :: style              ! type of coordinates 

    ! 'x3' for Cartesians, 
    ! 'xFrac' for fractionals
    ! default => cartesians

    ! Internal Variables
    !character(len=6) :: id1, id0
    character(len=20):: stylei
    integer          :: i

    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif

    call stmAddStartTag(xf, 'molecule', id, title, dictref)
    call xml_NewElement(xf, 'atomArray')
    !TOHW this should be cleaned up .... Internal writes should go through str
    do i = 1, natoms
! This next is obviously bollocks. It will result in duplicate ids.
       !write(id0, '(i4)') i
       !id0 = adjustl(id0)
       !id1 = 'a'
       !id1(2:) = id0
       !call cmlAddAtom(xf=xf, elem=elements(i), id=trim(id1))
! There is a case to be made for TRIMing the elements(i) and refs(i) strings
! because they are part of an array, and the user cannot pass in strings
! of varying length.
       call cmlAddAtom(xf=xf, elem=elements(i))
       if (present(refs)) call xml_AddAttribute(xf, 'ref', refs(i))
       if (stylei .eq. 'x3') then
          call CMLATX39DP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
       elseif (stylei .eq. 'xFrac') then
          call CMLATXF9DP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
       elseif (stylei .eq. 'xyz3') then
          call CMLATXYZ39DP(xf, coords(1, i), coords (2, i), coords(3, i), fmt)
       elseif (stylei .eq. 'xyzFrac') then
          call CMLATXYZFRACT9DP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
       endif
       call xml_EndElement(xf, 'atom')
    enddo

    call xml_EndElement(xf, 'atomArray')
    call xml_EndElement(xf, 'molecule')
    
  END SUBROUTINE cmlAddMoleculeDP

  
  ! -------------------------------------------------
  ! 2. writes complete SP molecule to xml channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddMoleculeSP(xf, natoms, elements, refs, coords, style, id, title, dictref, fmt)
    implicit none
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in)                    :: natoms          ! number of atoms
    character(len=*), intent(in)           :: elements(*)     ! chemical element types
    real(kind=sp), intent(in)              :: coords(3, *)    ! atomic coordinates
    character(len=*), intent(in), optional :: refs(natoms)    ! id
    character(len=*), intent(in), optional :: id              ! id
    character(len=*), intent(in), optional :: title           ! the title
    character(len=*), intent(in), optional :: dictref         ! the dictionary reference
    character(len=*), intent(in), optional :: fmt             ! format for coords
    character(len=*), intent(in), optional :: style           ! type of coordinates ('x3'for Cartesians, 'xFrac' 
    ! for fractionals; ' ' = default => cartesians)
    ! Flush on entry and exit
    !character(len=6) :: id1, id0
    integer          :: i
    character(len=10):: stylei

    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif

    call stmAddStartTag(xf, 'molecule', id, title, dictref)
    call xml_NewElement(xf, 'atomArray')
    do i = 1, natoms
       !write(id0, '(i4)') i
       !id0 = adjustl(id0)
       !id1 = 'a'
       !id1(2:) = id0
       !call cmlAddAtom(xf=xf, elem=elements(i), id=trim(id1))
       call cmlAddAtom(xf=xf, elem=elements(i))
       if (present(refs)) call xml_AddAttribute(xf, 'ref', refs(i))
       if (stylei .eq. 'x3') then
          call CMLATX39SP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
       elseif (stylei .eq. 'xFrac') then
          call CMLATXF9SP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
       elseif (stylei .eq. 'xyz3') then
          call CMLATXYZ39SP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
       elseif (stylei .eq. 'xyzFrac') then
          call CMLATXYZFRACT9SP(xf, coords(1, i), coords(2, i), coords(3, i), fmt)
       endif
       call xml_EndElement(xf, 'atom')
    enddo

    call xml_EndElement(xf, 'atomArray')
    call xml_EndElement(xf, 'molecule')
    
    
  END SUBROUTINE cmlAddMoleculeSP
  
  
  ! -------------------------------------------------
  ! 1. writes complete DP molecule to xml channel (No. 2)
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddMolecule3DP(xf, natoms, elements, x, y, z, style, id, title, dictref, fmt)
    implicit none
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in)                    :: natoms          ! number of atoms
    real(kind=dp), intent(in)               :: x(*)
    real(kind=dp), intent(in)               :: y(*)
    real(kind=dp), intent(in)               :: z(*)
    character(len=*), intent(in)           :: elements(*)     ! chemical element types
    character(len=*), intent(in), optional :: id              ! id
    character(len=*), intent(in), optional :: title           ! the title
    character(len=*), intent(in), optional :: dictref         ! the dictionary reference
    character(len=*), intent(in), optional :: fmt             ! format for coords
    character(len=*), intent(in), optional :: style           ! type of coordinates ('x3' for Cartesians, 'xFrac' 
    ! for fractionals; ' ' = default => cartesians)
    !character(len=6)  :: id1, id0
    integer           :: i
    character(len=10) :: stylei

    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif

    call stmAddStartTag(xf=xf, name='molecule', id=id, title=title, dictref=dictref)
    call xml_NewElement(xf, 'atomArray')

    do i = 1, natoms
       !write(id0, '(i4)') i
       !id0 = adjustl(id0)
       !id1 = 'a'
       !id1(2:) = id0
       !call cmlAddAtom(xf=xf, elem=elements(i), id=trim(id1))
       call cmlAddAtom(xf=xf, elem=elements(i))
       if (trim(stylei) .eq. 'x3') then
          call CMLATX39DP(xf, x(i), y(i), z(i), fmt)
       elseif (stylei .eq. 'xFrac') then
          call CMLATXF9DP(xf, x(i), y(i), z(i), fmt)
       elseif (stylei .eq. 'xyz3') then
          call CMLATXYZ39DP(xf, x(i), y(i), z(i), fmt)
       elseif (stylei .eq. 'xyzFrac') then
          call CMLATXYZFRACT9DP(xf, x(i), y(i), z(i), fmt)
       endif
       call xml_EndElement(xf, 'atom')
    enddo

    call xml_EndElement(xf, 'atomArray')
    call xml_EndElement(xf, 'molecule')
    
  END SUBROUTINE cmlAddMolecule3DP
  
  
  ! -------------------------------------------------
  ! 2. writes complete SP molecule to xml channel (No. 2)
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddMolecule3SP(xf, natoms, elements, x, y, z, style, id, title, dictref, fmt)


    implicit none
    type(xmlf_t), intent(inout) :: xf
    ! 10 Arguments
    integer, intent(in)                    :: natoms          ! number of atoms
    real(kind=sp), intent(in)               :: x(*)
    real(kind=sp), intent(in)               :: y(*)
    real(kind=sp), intent(in)               :: z(*)
    character(len=*), intent(in)           :: elements(*)      ! chemical element types
    character(len=*), intent(in), optional :: id               ! id
    character(len=*), intent(in), optional :: title            ! the title
    character(len=*), intent(in), optional :: dictref          ! the dictionary reference
    character(len=*), intent(in), optional :: fmt              ! format for coords
    character(len=*), intent(in), optional :: style            ! type of coordinates ('x3' for Cartesians, 'xFrac' 
    ! for fractionals; ' ' = default => cartesians)
    ! Internal variables
    !character(len=6)  :: id1, id0
    integer           :: i
    character(len=10) :: stylei

    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif

    call xml_NewElement(xf, 'molecule')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'title', title)
    call xml_AddAttribute(xf, 'dictref', dictref)
    call xml_NewElement(xf, 'atomArray')
    do i = 1, natoms
       !write(id0, '(i4)') i
       !id0 = adjustl(id0)
       !id1 = 'a'
       !id1(2:) = id0
       !call cmlAddAtom(xf=xf, elem=elements(i), id=trim(id1))
       call cmlAddAtom(xf=xf, elem=elements(i))
       if (stylei .eq. 'x3') then
          call CMLATX39SP(xf, x(i), y(i), z(i), fmt)
       else if (stylei .eq. 'xFrac') then
          call CMLATXF9SP(xf, x(i), y(i), z(i), fmt)
       else if (stylei .eq. 'xyz3') then
          call CMLATXYZ39SP(xf, x(i), y(i), z(i), fmt)
       else if (stylei .eq. 'xyzFrac') then
          call CMLATXYZFRACT9SP(xf, x(i), y(i), z(i), fmt)
       endif
           call xml_EndElement(xf, 'atom')
    enddo

    call xml_EndElement(xf, 'atomArray')
    call xml_EndElement(xf, 'molecule')

  END SUBROUTINE cmlAddMolecule3SP
  
  ! -------------------------------------------------
  ! writes an <atom> start tag
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddAtom(xf, elem, id, charge, hCount, occupancy, fmt)


    implicit none
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in), optional           :: charge     ! formalCharge
    integer, intent(in), optional           :: hCount     ! hydrogenCount
    real(kind=sp), intent(in), optional     :: occupancy  ! hydrogenCount
    character(len=*), intent(in), optional  :: elem       ! chemical element name
    character(len=*), intent(in), optional  :: id         ! atom id
    character(len=*), intent(in), optional  :: fmt        ! format

    call xml_NewElement(xf, 'atom')
    if (present(elem))      call xml_AddAttribute(xf, 'elementType', trim(elem))
    if (present(id))        call xml_AddAttribute(xf, 'id', id)
    if (present(charge))    call xml_AddAttribute(xf, 'formalCharge', charge)
    if (present(hCount))    call xml_AddAttribute(xf, 'hydrogenCount', hCount)
    if (present(occupancy)) call xml_AddAttribute(xf, 'occupancy', occupancy, fmt)

  END SUBROUTINE cmlAddAtom
  
  
  ! -------------------------------------------------
  ! 1. append SP coordinates to atom tag
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddCoordinatesSP(xf, x, y, z, style, fmt)
    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)               :: x, y
    real(kind=sp), intent(in), optional     :: z
    character(len=*), intent(in), optional :: style
    character(len=*), intent(in), optional :: fmt

    ! Internal variable
    character(len=10):: stylei

    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif

    if (present(z) .and. stylei .eq. 'x3') then
       call CMLATX39SP(xf, x, y, z, fmt)
    else if (present(z) .and. stylei .eq. 'xFrac') then
       call CMLATXF9SP(xf, x, y, z, fmt)
    else if (present(z) .and. stylei .eq. 'xyz3') then
       call CMLATXYZ39SP(xf, x, y, z, fmt)
    else if (present(z) .and. stylei .eq. 'xyzFrac') then
       call CMLATXYZFRACT9SP(xf, x, y, z, fmt)
    elseif (.not. present(z) .and. stylei .eq. 'xy2') then
       call CMLATXY9SP(xf, x, y, fmt)
    endif

  END SUBROUTINE cmlAddCoordinatesSP

  ! -------------------------------------------------
  ! 2. append DP coordinates to atom tag
  ! -------------------------------------------------

  SUBROUTINE cmlAddCoordinatesDP(xf, x, y, z, style, fmt)
    implicit none
    type(xmlf_t), intent(inout) :: xf 
    real(kind=dp), intent(in)               :: x, y
    real(kind=dp), intent(in), optional     :: z
    character(len=*), intent(in), optional :: style
    character(len=*), intent(in), optional :: fmt
    
    ! Internal variable
    character(len=10):: stylei

    if (present(style)) then
       stylei = style
    else
       stylei = 'x3'
    endif
    
    if (present(z) .and. stylei .eq. 'x3') then
       call CMLATX39DP(xf, x, y, z, fmt)
    else if (present(z) .and. stylei .eq. 'xFrac') then
       call CMLATXF9DP(xf, x, y, z, fmt)
    else if (present(z) .and. stylei .eq. 'xyz3') then
       call CMLATXYZ39DP(xf, x, y, z, fmt)
    else if (present(z) .and. stylei .eq. 'xyzFrac') then
       call CMLATXYZFRACT9DP(xf, x, y, z, fmt)
    else if (.not. present(z) .and. stylei .eq. 'xy2') then
       call CMLATXY9DP(xf, x, y, fmt)           
    endif
    
  END SUBROUTINE cmlAddCoordinatesDP

  
  ! -------------------------------------------------
  ! 1. writes a DP <length> element to output channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddLengthDP(xf, length, id, atomRef1, atomRef2, fmt)
    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), intent(in)     :: length     ! length
    character(len=*), intent(in) :: id         ! length id
    character(len=*), intent(in) :: atomRef1   ! ref to first atom
    character(len=*), intent(in) :: atomRef2   ! ref to second atom
    character(len=*), intent(in), optional :: fmt        ! format

    ! Flush on entry and exit
    call CMLLEN9DP(xf, id, atomRef1, atomRef2, length, fmt)
  END SUBROUTINE cmlAddLengthDP

  ! -------------------------------------------------
  ! 2. writes a SP <length> element to output channel
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddLengthSP(xf, length, id, atomRef1, atomRef2, fmt)

    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)     :: length     ! the length
    character(len=*), intent(in) :: id         ! length id
    character(len=*), intent(in) :: atomRef1   ! ref to first atom
    character(len=*), intent(in) :: atomRef2   ! ref to second atom
    character(len=*), intent(in), optional :: fmt        ! format

    ! Flush on entry and exit
    call CMLLEN9SP(xf, id, atomRef1, atomRef2, length, fmt)
  END SUBROUTINE cmlAddLengthSP


  ! -------------------------------------------------
  ! 1. writes an DP <angle> element to output channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddAngleDP(xf, angle, id, atomRef1, atomRef2, atomRef3, fmt)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), intent(in)     :: angle        ! the angle
    character(len=*), intent(in) :: id           ! angle id
    character(len=*), intent(in) :: atomRef1     ! ref to first atom
    character(len=*), intent(in) :: atomRef2     ! ref to second atom
    character(len=*), intent(in) :: atomRef3     ! ref to third atom
    character(len=*), intent(in), optional :: fmt          ! format

    ! Flush on entry and exit
    call CMLANG9DP(xf, id, atomRef1, atomRef2, atomRef3, angle, fmt)
  END SUBROUTINE cmlAddAngleDP

  ! -------------------------------------------------
  ! 2. writes an SP <angle> element to output channel
  ! -------------------------------------------------

  SUBROUTINE cmlAddAngleSP(xf, angle, id, atomRef1, atomRef2, atomRef3, fmt)


    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)     :: angle        ! the angle
    character(len=*), intent(in) :: id           ! angle id
    character(len=*), intent(in) :: atomRef1     ! ref to first atom
    character(len=*), intent(in) :: atomRef2     ! ref to second atom
    character(len=*), intent(in) :: atomRef3     ! ref to third atom
    character(len=*), intent(in), optional :: fmt          ! format

    ! Flush on entry and exit
    call CMLANG9SP(xf, id, atomRef1, atomRef2, atomRef3, angle, fmt)
  END SUBROUTINE cmlAddAngleSP


  ! -------------------------------------------------
  ! 1. creates and writes a DP <torsion> element
  ! -------------------------------------------------

  SUBROUTINE cmlAddTorsionDP(xf, torsion, id, atomRef1, atomRef2, atomRef3, atomRef4, fmt)


    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), intent(in)     :: torsion         ! the torsion
    character(len=*), intent(in) :: id              ! torsion id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    character(len=*), intent(in) :: atomRef4        ! ref to fourth atom
    character(len=*), intent(in), optional :: fmt             ! format

    ! Flush on entry and exit
    call CMLTOR9DP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, fmt)
  END SUBROUTINE cmlAddTorsionDP
  
  ! -------------------------------------------------
  ! 2. creates and writes a SP <torsion> element
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddTorsionSP(xf, torsion, id, atomRef1, atomRef2, atomRef3, atomRef4, fmt)


    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)     :: torsion         ! the torsion
    character(len=*), intent(in) :: id              ! torsion id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    character(len=*), intent(in) :: atomRef4        ! ref to fourth atom
    character(len=*), intent(in), optional :: fmt             ! format

    ! Flush on entry and exit
    call CMLTOR9SP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, fmt)
  END SUBROUTINE cmlAddTorsionSP


  ! -------------------------------------------------
  ! 1. creates and writes an SP Lattice element
  ! -------------------------------------------------

  SUBROUTINE cmlAddLatticeSP(xf, cell, units, title, id, dictref, conv, lattType, spaceType, fmt)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)               :: cell(3,3)
    character(len=*), intent(in), optional :: units       
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! convention
    character(len=*), intent(in), optional :: lattType 
    character(len=*), intent(in), optional :: spaceType    !
    character(len=*), intent(in), optional :: fmt         

    integer :: i
    character(len=10) :: formt
    if (present(fmt)) then
       formt = fmt
    else
       formt = spformat
    endif

    call xml_NewElement(xf, 'lattice')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lattType)) call xml_AddAttribute(xf, 'latticeType', lattType)
    if (present(spaceType)) call xml_AddAttribute(xf, 'spaceType', spaceType)

    do i = 1,3
       call xml_NewElement(xf, 'latticeVector')
       if (present(units)) call xml_AddAttribute(xf, 'units', units)
       call xml_AddAttribute(xf, 'dictRef', 'cml:latticeVector')
       call xml_AddPcdata(xf, cell(1,i), formt)
       call xml_AddPcdata(xf, cell(2,i), formt, space=.true.)
       call xml_AddPcdata(xf, cell(3,i), formt, space=.true.)
       call xml_EndElement(xf, 'latticeVector')
    enddo
    call xml_EndElement(xf, 'lattice')
    
  END SUBROUTINE cmlAddLatticeSP


  ! -------------------------------------------------
  ! 2. creates and writes DP Lattice element
  ! -------------------------------------------------

  SUBROUTINE cmlAddLatticeDP(xf, cell, units, title, id, dictref, conv, lattType, spaceType, fmt)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), intent(in)               :: cell(3,3)
    character(len=*), intent(in), optional :: units       
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! 
    character(len=*), intent(in), optional :: lattType     ! 
    character(len=*), intent(in), optional :: spaceType    !
    character(len=*), intent(in), optional :: fmt         

    integer :: i
    character(len=10) :: formt
    if (present(fmt)) then
       formt = fmt
    else
       formt = dpformat
    endif

    call xml_NewElement(xf, 'lattice')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lattType)) call xml_AddAttribute(xf, 'latticeType', lattType)
    if (present(spaceType)) call xml_AddAttribute(xf, 'spaceType', spaceType)

    do i = 1,3
       call xml_NewElement(xf, 'latticeVector')
       if (present(units)) call xml_AddAttribute(xf, 'units', units)
       call xml_AddAttribute(xf, 'dictRef', 'cml:latticeVector')
       call xml_AddPcdata(xf, cell(1,i), formt)
       call xml_AddPcdata(xf, cell(2,i), formt, space=.true.)
       call xml_AddPcdata(xf, cell(3,i), formt, space=.true.)
       call xml_EndElement(xf, 'latticeVector')
    enddo
    call xml_EndElement(xf, 'lattice')

  END SUBROUTINE cmlAddLatticeDP


  ! -------------------------------------------------
  ! 1. creates and writes a DP <cell> element
  ! -------------------------------------------------

  SUBROUTINE cmlAddCrystalDP(xf, a, b, c, alpha, beta, gamma, id, title, dictref, conv, lenunits, angunits, spaceType, fmt)
    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), intent(in)               :: a, b, c      ! cell parameters
    real(kind=dp), intent(in)               :: alpha        ! alpha cell parameter
    real(kind=dp), intent(in)               :: beta         ! beta cell parameter
    real(kind=dp), intent(in)               :: gamma        ! gamma cell parameter
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! convention
    character(len=*), intent(in), optional :: lenunits     ! units for length (default = angstrom)
    character(len=*), intent(in), optional :: angunits     ! units for angles (default = degree)
    character(len=*), intent(in), optional :: spaceType    ! spacegroup
    character(len=*), intent(in), optional :: fmt          ! format

    ! Flush on entry and exit
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = dpformat
    endif

    call xml_NewElement(xf=xf, name='crystal')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(lenunits)) then
      call stmAddScalar(xf=xf, value=a, title='a', dictref='cml:a', units=lenunits, fmt=formt)
      call stmAddScalar(xf=xf, value=b, title='b', dictref='cml:b', units=lenunits, fmt=formt)
      call stmAddScalar(xf=xf, value=c, title='c', dictref='cml:c', units=lenunits, fmt=formt)
    else
      call stmAddScalar(xf=xf, value=a, title='a', dictref='cml:a', units=U_ANGSTR, fmt=formt)
      call stmAddScalar(xf=xf, value=b, title='b', dictref='cml:b', units=U_ANGSTR, fmt=formt)
      call stmAddScalar(xf=xf, value=c, title='c', dictref='cml:c', units=U_ANGSTR, fmt=formt)
    endif
    if (present(angunits)) then
      call stmAddScalar(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=angunits, fmt=formt)
      call stmAddScalar(xf=xf, value=beta,  title='beta',  dictref='cml:beta',  units=angunits, fmt=formt)
      call stmAddScalar(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=angunits, fmt=formt)
    else
      call stmAddScalar(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=U_DEGREE, fmt=formt)
      call stmAddScalar(xf=xf, value=beta,  title='beta',  dictref='cml:beta',  units=U_DEGREE, fmt=formt)
      call stmAddScalar(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=U_DEGREE, fmt=formt)
    endif
    if (present(spaceType)) then
      call xml_NewElement(xf, 'symmetry')
      call xml_AddAttribute(xf, 'spaceGroup', spaceType)
      call xml_EndElement(xf, 'symmetry')
    endif
    call xml_EndElement(xf, 'crystal')

  END SUBROUTINE cmlAddCrystalDP

  ! -------------------------------------------------
  ! 2. creates and writes a SP <cell> element
  ! -------------------------------------------------

  SUBROUTINE cmlAddCrystalSP(xf, a, b, c, alpha, beta, gamma, id, title, dictref, conv, lenunits, angunits, spaceType, fmt)
    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp), intent(in)     :: a, b, c      ! cell parameters
    real(kind=sp), intent(in)     :: alpha        ! alpha cell parameter
    real(kind=sp), intent(in)     :: beta         ! beta cell parameter
    real(kind=sp), intent(in)     :: gamma        ! gamma cell parameter
    character(len=*), intent(in), optional :: id           ! id
    character(len=*), intent(in), optional :: title        ! title
    character(len=*), intent(in), optional :: dictref      ! dictref
    character(len=*), intent(in), optional :: conv         ! convention
    character(len=*), intent(in), optional :: lenunits     ! units for length (' ' = angstrom)
    character(len=*), intent(in), optional :: angunits     ! units for angles (' ' = degree)
    character(len=*), intent(in), optional :: spaceType    ! spacegroup
    character(len=*), intent(in), optional :: fmt          ! format

    ! Flush on entry and exit
    character(len=30) :: lunits, aunits
    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = spformat
    endif
    if (present(lenunits)) then
       lunits = lenunits
    else
       lunits = U_ANGSTR
    endif
    if (present(angunits)) then
       aunits = angunits
    else
       aunits = U_DEGREE
    endif

    call xml_NewElement(xf=xf, name='crystal')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    call stmAddScalar(xf=xf, value=a, title='a', dictref='cml:a', units=lunits, fmt=formt)
    call stmAddScalar(xf=xf, value=b, title='b', dictref='cml:b', units=lunits, fmt=formt)
    call stmAddScalar(xf=xf, value=c, title='c', dictref='cml:c', units=lunits, fmt=formt)
    call stmAddScalar(xf=xf, value=alpha, title='alpha', dictref='cml:alpha', units=aunits, fmt=formt)
    call stmAddScalar(xf=xf, value=beta,  title='beta',  dictref='cml:beta', units=aunits, fmt=formt)
    call stmAddScalar(xf=xf, value=gamma, title='gamma', dictref='cml:gamma', units=aunits, fmt=formt)
    if (present(spaceType)) then
      call xml_NewElement(xf, 'symmetry')
      call xml_AddAttribute(xf, 'spaceGroup', spaceType)
      call xml_EndElement(xf, 'symmetry')
    endif
    call xml_EndElement(xf, 'crystal')


  END SUBROUTINE cmlAddCrystalSP
  
  
  ! -------------------------------------------------
  ! 1. creates and writes an DP <eigen> element
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddEigenvalueDP(xf, n, eigvec, eigval, id, title, dictref, fmt)


    implicit none
    type(xmlf_t), intent(inout)            :: xf
    integer, intent(in)                    :: n              ! number of elements
    real(kind=dp), intent(in)              :: eigvec(n, *)   ! eigenvectors
    real(kind=dp), intent(in)              :: eigval(*)      ! eigenvalues
    character(len=*), intent(in), optional :: id             ! id
    character(len=*), intent(in), optional :: title          ! title
    character(len=*), intent(in), optional :: dictref        ! dictionary reference
    character(len=*), intent(in), optional :: fmt            ! format
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = dpformat
    endif

    ! Flush on entry and exit
    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictRef)
    call stmAddArray(xf=xf, nvalue=n, array=eigval, title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddMatrix(xf=xf, ncols=n, nrows=n, matrix=eigvec, title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')

  END SUBROUTINE cmlAddEigenvalueDP



  ! -------------------------------------------------
  ! 2. creates and writes an SP <eigen> element
  ! -------------------------------------------------
  
  SUBROUTINE cmlAddEigenvalueSP(xf, n, eigvec, eigval, id, title, dictref, fmt)


    implicit none
    type(xmlf_t), intent(inout) :: xf
    integer, intent(in)          :: n              ! number of elements
    real(kind=sp), intent(in)     :: eigvec(n, *) ! eigenvectors
    real(kind=sp), intent(in)     :: eigval(*)      ! eigenvalues
    character(len=*), intent(in), optional :: id             ! id
    character(len=*), intent(in), optional :: title          ! title
    character(len=*), intent(in), optional :: dictref        ! dictionary reference
    character(len=*), intent(in), optional :: fmt            ! format
    character(len=10):: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = spformat
    endif

    ! Flush on entry and exit
    call xml_NewElement(xf, 'eigen')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictRef))   call xml_AddAttribute(xf, 'dictRef', dictref)
    call stmAddArray(xf=xf, nvalue=n, array=eigval, title='eigenvalues', dictref=dictRef, fmt=fmt)
    call stmAddMatrix(xf=xf, ncols=n, nrows=n, matrix=eigvec, title='eigenvectors', fmt=fmt)
    call xml_EndElement(xf, 'eigen')

  END SUBROUTINE cmlAddEigenvalueSP


  subroutine cmlAddMetadataCh(xf, name, content, conv)
    
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: content
    character(len=*), intent(in), optional :: conv
    
    call xml_NewElement(xf, 'metadata')
    call xml_AddAttribute(xf, 'name', name)
    call xml_AddAttribute(xf, 'content', content)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    call xml_EndElement(xf, 'metadata')

  end subroutine cmlAddMetadataCh

  subroutine cmlAddMetadataI(xf, name, content, conv)
    
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    integer, intent(in) :: content
    character(len=*), intent(in), optional :: conv
    
    call xml_NewElement(xf, 'metadata')
    call xml_AddAttribute(xf, 'name', name)
    call xml_AddAttribute(xf, 'content', content)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    call xml_EndElement(xf, 'metadata')
    
  end subroutine cmlAddMetadataI

  subroutine cmlAddMetadataLG(xf, name, content, conv)

     type(xmlf_t), intent(inout) :: xf
     character(len=*), intent(in) :: name
     logical, intent(in) :: content
     character(len=*), intent(in), optional :: conv

     call xml_NewElement(xf, 'metadata')
     call xml_AddAttribute(xf, 'name', name)
     call xml_AddAttribute(xf, 'content', content)
     if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
     call xml_EndElement(xf, 'metadata')

   end subroutine cmlAddMetadataLG



  ! -------------------------------------------------
  ! 1. creates and writes an Char <parameter> element
  ! -------------------------------------------------


  SUBROUTINE cmlAddParameterCh(xf, value, ref, id, title, conv, cons, units, name, role, dictref, datatype)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    character(len=*) :: value 
    character(len=*), optional :: ref 
    character(len=*), optional :: title
    character(len=*), optional :: id
    character(len=*), optional :: conv
    character(len=*), optional :: cons
    character(len=*), optional :: units
    character(len=*), optional :: name
    character(len=*), optional :: role
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: datatype

    call xml_NewElement(xf, 'parameter')
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons))    call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name))    call xml_AddAttribute(xf, 'name', name)
    if (present(role))    call xml_AddAttribute(xf, 'role', role)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)

    call xml_NewElement(xf, 'scalar')
    if (present(units)) call xml_AddAttribute(xf, 'units', units)

    if (present(datatype)) then
        call xml_AddAttribute(xf, 'dataType', datatype)
    else
        call xml_AddAttribute(xf, 'dataType', "xsd:string")
    endif

    call xml_AddPcdata(xf, value)
    call xml_EndElement(xf, 'scalar')
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERCH


  ! -------------------------------------------------
  ! 2. creates and writes an SP <parameter> element
  ! -------------------------------------------------


  SUBROUTINE cmlAddParameterSP(xf, value, ref, title, id, conv, cons, units, name, role, dictref, fmt)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=sp) :: value 
    character(len=*), optional :: ref 
    character(len=*), optional :: title
    character(len=*), optional :: id
    character(len=*), optional :: conv
    character(len=*), optional :: cons
    character(len=*), optional :: units
    character(len=*), optional :: name
    character(len=*), optional :: role
    character(len=*), optional :: fmt
    character(len=*), intent(in), optional :: dictref

    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = spformat
    endif

    call xml_NewElement(xf, 'parameter')
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons))    call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name))    call xml_AddAttribute(xf, 'name', name)
    if (present(role))    call xml_AddAttribute(xf, 'role', role)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    call xml_NewElement(xf, 'scalar')
    if (present(units)) call xml_AddAttribute(xf, 'units', units)
    call xml_AddAttribute(xf, 'dataType', 'xsd:float')
    call xml_AddPcdata(xf, value, formt)
    call xml_EndElement(xf, 'scalar')
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERSP


  ! -------------------------------------------------
  ! 3. creates and writes an DP <parameter> element
  ! -------------------------------------------------


  SUBROUTINE cmlAddParameterDP(xf, value, ref, title, id, conv, cons, units, name, role, dictref, fmt)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp) :: value 
    character(len=*), optional :: ref 
    character(len=*), optional :: title
    character(len=*), optional :: id
    character(len=*), optional :: conv
    character(len=*), optional :: cons
    character(len=*), optional :: units
    character(len=*), optional :: name
    character(len=*), optional :: role
    character(len=*), intent(in), optional :: dictref
    character(len=*), optional :: fmt    

    character(len=10) :: formt

    if (present(fmt)) then
       formt = fmt
    else
       formt = dpformat
    endif

    call xml_NewElement(xf, 'parameter')
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons))    call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name))    call xml_AddAttribute(xf, 'name', name)
    if (present(role))    call xml_AddAttribute(xf, 'role', role)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    call xml_NewElement(xf, 'scalar')
    call xml_AddAttribute(xf, 'dataType', 'xsd:double')
    if (present(units)) call xml_AddAttribute(xf, 'units', units)
    call xml_AddPcdata(xf, value, formt)
    call xml_EndElement(xf, 'scalar')
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERDP


  ! -------------------------------------------------
  ! 4. creates and writes an Integer <parameter> element
  ! -------------------------------------------------


  SUBROUTINE cmlAddParameterI(xf, value, ref, id, title, conv, cons, units, name, role, dictref)

    implicit none
    type(xmlf_t), intent(inout) :: xf
    integer :: value 
    character(len=*), optional :: ref 
    character(len=*), optional :: title
    character(len=*), optional :: id
    character(len=*), optional :: conv
    character(len=*), optional :: cons
    character(len=*), optional :: units
    character(len=*), optional :: name
    character(len=*), optional :: role
    character(len=*), intent(in), optional :: dictref

    call xml_NewElement(xf, 'parameter')
    if (present(ref))   call xml_AddAttribute(xf, 'ref', ref)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(id))    call xml_AddAttribute(xf, 'id', id)
    if (present(conv))  call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons))  call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name))  call xml_AddAttribute(xf, 'name', name)
    if (present(role))  call xml_AddAttribute(xf, 'role', role)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    call xml_NewElement(xf, 'scalar')
    call xml_AddAttribute(xf, 'dataType', 'xsd:integer')
    if (present(units)) call xml_AddAttribute(xf, 'units', units)
    call xml_AddPcdata(xf, value)
    call xml_EndElement(xf, 'scalar')
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERI


  SUBROUTINE cmlAddParameterLG(xf, value, ref, id, title, conv, cons, units, name, role, dictref)

    implicit none
    type(xmlf_t),     intent(inout)        :: xf
    logical,          intent(in)           :: value 
    character(len=*), intent(in), optional :: ref 
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: cons
    character(len=*), intent(in), optional :: units
    character(len=*), intent(in), optional :: name
    character(len=*), intent(in), optional :: role
    character(len=*), intent(in), optional :: dictref

    call xml_NewElement(xf, 'parameter')
    if (present(ref))     call xml_AddAttribute(xf, 'ref', ref)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    if (present(cons))    call xml_AddAttribute(xf, 'constraint', cons)
    if (present(name))    call xml_AddAttribute(xf, 'name', name)
    if (present(role))    call xml_AddAttribute(xf, 'role', role)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    call xml_NewElement(xf, 'scalar')
    call xml_AddAttribute(xf, 'dataType', 'xsd:boolean')
    call xml_AddPcdata(xf, value)
    call xml_EndElement(xf, 'scalar')
    call xml_EndElement(xf, 'parameter')

  END SUBROUTINE CMLADDPARAMETERLG




! =================================================
! basic CML routines
! =================================================

  
  ! -------------------------------------------------
  ! 1. adds DP xyz3 to start tag
  ! -------------------------------------------------
  
  SUBROUTINE CMLATXYZ39DP(xf, x3, y3, z3, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=dp), intent(in)               :: x3, y3, z3 ! coordinates
    character(len=*), intent(in), optional  :: fmt        ! format

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'xyz3', str(x3,fmt)//' '//str(y3,fmt)//' '//str(z3,fmt) )
    else
      call xml_AddAttribute(xf, 'xyz3', str(x3)//' '//str(y3)//' '//str(z3) )
    endif

  END SUBROUTINE CMLATXYZ39DP


  ! -------------------------------------------------
  ! 2. adds SP xyz3 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXYZ39SP(xf, x3, y3, z3, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=sp), intent(in)               :: x3, y3, z3 ! coordinates
    character(len=*), intent(in), optional  :: fmt

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'xyz3', str(x3,fmt)//' '//str(y3,fmt)//' '//str(z3,fmt) )
    else
      call xml_AddAttribute(xf, 'xyz3', str(x3)//' '//str(y3)//' '//str(z3) )
    endif

  END SUBROUTINE CMLATXYZ39SP
  
  ! -------------------------------------------------
  ! 1. adds DP xyzFrac to start tag
  ! -------------------------------------------------
  
  SUBROUTINE CMLATXYZFRACT9DP(xf, x3, y3, z3, fmt)
    type(xmlf_t), intent(inout)             :: xf
    real(kind=dp), intent(in)               :: x3, y3, z3 ! coordinates
    character(len=*), intent(in), optional  :: fmt        ! format

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'xyzFrac', str(x3,fmt)//' '//str(y3,fmt)//' '//str(z3,fmt) )
    else
      call xml_AddAttribute(xf, 'xyzFrac', str(x3)//' '//str(y3)//' '//str(z3) )
    endif

  END SUBROUTINE CMLATXYZFRACT9DP

  ! -------------------------------------------------
  ! 2. adds SP xyzFrac to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXYZFRACT9SP(xf, x3, y3, z3, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: x3, y3, z3 ! coordinates
    character(len=*), intent(in), optional :: fmt        ! format

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'xyzFrac', str(x3,fmt)//' '//str(y3,fmt)//' '//str(z3,fmt) )
    else
      call xml_AddAttribute(xf, 'xyzFrac', str(x3)//' '//str(y3)//' '//str(z3) )
    endif

  END SUBROUTINE CMLATXYZFRACT9SP


  ! -------------------------------------------------
  ! 1. adds DP x3, y3, z3 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATX39DP(xf, x3, y3, z3, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: x3, y3, z3 ! coordinates
    character(len=*), intent(in), optional :: fmt

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'x3', str(x3,fmt))
      call xml_AddAttribute(xf, 'y3', str(y3,fmt))
      call xml_AddAttribute(xf, 'z3', str(z3,fmt))
    else
      call xml_AddAttribute(xf, 'x3', x3)
      call xml_AddAttribute(xf, 'y3', y3)
      call xml_AddAttribute(xf, 'z3', z3)
    endif

  END SUBROUTINE CMLATX39DP

  ! -------------------------------------------------
  ! 2. adds SP x3, y3, z3 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATX39SP(xf, x3, y3, z3, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: x3, y3, z3 ! coordinates
    character(len=*), intent(in), optional :: fmt

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'x3', str(x3,fmt))
      call xml_AddAttribute(xf, 'y3', str(y3,fmt))
      call xml_AddAttribute(xf, 'z3', str(z3,fmt))
    else
      call xml_AddAttribute(xf, 'x3', x3)
      call xml_AddAttribute(xf, 'y3', y3)
      call xml_AddAttribute(xf, 'z3', z3)
    endif

  END SUBROUTINE CMLATX39SP


  ! -------------------------------------------------
  ! 1. adds DP xFract, yFract, zFract to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXF9DP(xf, xFract, yFract, zFract, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: xFract, yFract, zFract ! coordinates
    character(len=*), intent(in), optional :: fmt

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'xFract', str(xFract,fmt))
      call xml_AddAttribute(xf, 'yFract', str(yFract,fmt))
      call xml_AddAttribute(xf, 'zFract', str(zFract,fmt))
    else
      call xml_AddAttribute(xf, 'xFract', xFract)
      call xml_AddAttribute(xf, 'yFract', yFract)
      call xml_AddAttribute(xf, 'zFract', zFract)
    endif

  END SUBROUTINE CMLATXF9DP
  
  ! -------------------------------------------------
  ! 2. adds SP xfrac, yFract, zFract to start tag
  ! -------------------------------------------------
  
  SUBROUTINE CMLATXF9SP(xf, xFract, yFract, zFract, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: xFract, yFract, zFract   ! fractional coordinates
    character(len=*), intent(in), optional :: fmt

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'xFract', str(xFract,fmt))
      call xml_AddAttribute(xf, 'yFract', str(yFract,fmt))
      call xml_AddAttribute(xf, 'zFract', str(zFract,fmt))
    else
      call xml_AddAttribute(xf, 'xFract', xFract)
      call xml_AddAttribute(xf, 'yFract', yFract)
      call xml_AddAttribute(xf, 'zFract', zFract)
    endif

  END SUBROUTINE CMLATXF9SP


  ! -------------------------------------------------
  ! 1. adds DP x2, y2 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXY9DP(xf, x2, y2, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=dp), intent(in)              :: x2, y2   ! coordinates
    character(len=*), intent(in), optional :: fmt      

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'x2', str(x2,fmt))
      call xml_AddAttribute(xf, 'y2', str(y2,fmt))
    else
      call xml_AddAttribute(xf, 'x2', x2)
      call xml_AddAttribute(xf, 'y2', y2)
    endif

  END SUBROUTINE CMLATXY9DP

  ! -------------------------------------------------
  ! 2. adds SP x2, y2 to start tag
  ! -------------------------------------------------

  SUBROUTINE CMLATXY9SP(xf, x2, y2, fmt)
    type(xmlf_t), intent(inout)            :: xf
    real(kind=sp), intent(in)              :: x2, y2   ! coordinates
    character(len=*), intent(in), optional :: fmt 

    if (present(fmt)) then
      call xml_AddAttribute(xf, 'x2', str(x2,fmt))
      call xml_AddAttribute(xf, 'y2', str(y2,fmt))
    else
      call xml_AddAttribute(xf, 'x2', x2)
      call xml_AddAttribute(xf, 'y2', y2)
    endif

  END SUBROUTINE CMLATXY9SP


  ! -------------------------------------------------
  ! 1. creates a DP <length> element
  ! -------------------------------------------------

  SUBROUTINE CMLLEN9DP(xf, id, atomRef1, atomRef2, length, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id           ! length id
    character(len=*), intent(in) :: atomRef1     ! ref to first atom
    character(len=*), intent(in) :: atomRef2     ! ref to second atom
    real(kind=dp), intent(in)    :: length       ! the length
    character(len=*), intent(in), optional :: fmt          ! format

    call xml_NewElement(xf, 'length')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs2', adjustl(trim(atomRef1))//' '//adjustl(trim(atomRef2)))
    call xml_AddPcdata(xf, length, fmt)
    call xml_EndElement(xf, 'length')

  END SUBROUTINE CMLLEN9DP
  
  ! -------------------------------------------------
  ! 2. creates a SP <length> element
  ! -------------------------------------------------
  
  SUBROUTINE CMLLEN9SP(xf, id, atomRef1, atomRef2, length, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id           ! length id
    character(len=*), intent(in) :: atomRef1     ! ref to first atom
    character(len=*), intent(in) :: atomRef2     ! ref to second atom
    real(kind=sp), intent(in)    :: length       ! the length
    character(len=*), intent(in), optional :: fmt          ! format

    call xml_NewElement(xf, 'length')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs2', adjustl(trim(atomRef1))//' '//adjustl(trim(atomRef2)))
    call xml_AddAttribute(xf, 'atomRefs2', temp)
    call xml_AddPcdata(xf, length, fmt)
    call xml_EndElement(xf, 'length')

  END SUBROUTINE CMLLEN9SP


  ! -------------------------------------------------
  ! 1. creates a DP <angle> element
  ! -------------------------------------------------

  SUBROUTINE CMLANG9DP(xf, id, atomRef1, atomRef2, atomRef3, angle, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id              ! angle id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    real(kind=dp), intent(in)     :: angle          ! the angle
    character(len=*), intent(in), optional :: fmt             ! format

    call xml_NewElement(xf, 'angle')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs3', adjustl(trim(atomRef1))//' '//adjustl(trim(atomRef2))//' '//adjustl(trim(atomRef3)))
    call xml_AddPcdata(xf, angle, fmt)
    call xml_EndElement(xf, 'angle')

  END SUBROUTINE CMLANG9DP

  ! -------------------------------------------------
  ! 2. creates a SP <angle> element
  ! -------------------------------------------------

  SUBROUTINE CMLANG9SP(xf, id, atomRef1, atomRef2, atomRef3, angle, fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: id              ! angle id
    character(len=*), intent(in) :: atomRef1        ! ref to first atom
    character(len=*), intent(in) :: atomRef2        ! ref to second atom
    character(len=*), intent(in) :: atomRef3        ! ref to third atom
    real(kind=sp), intent(in)     :: angle          ! the angle
    character(len=*), intent(in), optional :: fmt             ! format

    call xml_NewElement(xf, 'angle')
    call xml_AddAttribute(xf, 'id', id)
    call xml_AddAttribute(xf, 'atomRefs3', adjustl(trim(atomRef1))//' '//adjustl(trim(atomRef2))//' '//adjustl(trim(atomRef3)))
    call xml_AddPcdata(xf, angle, fmt)
    call xml_EndElement(xf, 'angle')

  END SUBROUTINE CMLANG9SP


  ! -------------------------------------------------
  ! 1. creates a DP <torsion> element
  ! -------------------------------------------------
  
  SUBROUTINE CMLTOR9DP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, fmt)
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
         adjustl(trim(atomRef1))//' '//adjustl(trim(atomRef2))//' '//adjustl(trim(atomRef3))//' '//adjustl(trim(atomRef4)))
    call xml_AddPcdata(xf, torsion, fmt)
    call xml_EndElement(xf, 'torsion')

  END SUBROUTINE CMLTOR9DP

  ! -------------------------------------------------
  ! 2. creates a SP <torsion> element
  ! -------------------------------------------------

  SUBROUTINE CMLTOR9SP(xf, id, atomRef1, atomRef2, atomRef3, atomRef4, torsion, fmt)
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
         adjustl(trim(atomRef1))//' '//adjustl(trim(atomRef2))//' '//adjustl(trim(atomRef3))//' '//adjustl(trim(atomRef4)))
    call xml_AddPcdata(xf, torsion, fmt)
    call xml_EndElement(xf, 'torsion')

  END SUBROUTINE CMLTOR9SP

end module m_wcml_core
