module FoX_wcml

  use m_wcml_core
  use m_wcml_coma
  use m_wcml_geometry
  use m_wcml_lattice
  use m_wcml_metadata
  use m_wcml_molecule
  use m_wcml_parameter
  use m_wcml_property
  use m_wcml_stml

  implicit none
  private

  public :: cmlStartCml
  public :: cmlEndCml

  public :: cmlStartModule
  public :: cmlEndModule

  public :: cmlStartStep
  public :: cmlEndStep

  public :: cmlAddLength
  public :: cmlAddAngle
  public :: cmlAddTorsion

  public :: cmlAddCrystal
  public :: cmlAddLattice

  public :: cmlStartPropertyList
  public :: cmlEndPropertyList
  public :: cmlAddProperty

  public :: cmlStartParameterList
  public :: cmlEndParameterList
  public :: cmlAddParameter

  public :: cmlStartMetadataList
  public :: cmlEndMetadataList
  public :: cmlAddMetadata

  public :: cmlAddMolecule

  public :: cmlAddBand
  public :: cmlStartBandList
  public :: cmlEndBandList

  public :: cmlAddKPoint
  public :: cmlStartKPointList
  public :: cmlEndKPointList

  public :: cmlAddEigenvalue

end module FoX_wcml
