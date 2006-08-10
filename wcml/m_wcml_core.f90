module m_wcml_core

  use m_common_error, only: FoX_error

  use FoX_common, only: FoX_version
  use FoX_wxml, only: xmlf_t, xml_OpenFile, xml_Close
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement, xml_DeclareNamespace
  use FoX_wxml, only: xmlf_Name, xmlf_OpenTag

  use m_wcml_metadata, only: cmlAddMetadata

  implicit none
  private

  public :: cmlBeginFile
  public :: cmlFinishFile
  public :: cmlAddNamespace
  
  public :: cmlStartCml
  public :: cmlEndCml

contains

  subroutine cmlBeginFile(xf, filename, replace)
    type(xmlf_t), intent(out) :: xf
    character(len=*), intent(in) :: filename
    logical, intent(in), optional :: replace

    call xml_OpenFile(filename, xf, broken_indenting=.true., replace=replace)

  end subroutine cmlBeginFile

  
  subroutine cmlFinishFile(xf)
    type(xmlf_t), intent(inout) :: xf

    call xml_Close(xf)

  end subroutine cmlFinishFile


  subroutine cmlAddNamespace(xf, prefix, URI)
    type(xmlf_t), intent(inout) :: xf
    
    character(len=*), intent(in) :: prefix
    character(len=*), intent(in) :: URI

    if (xmlf_OpenTag(xf) /= "") &
      call FoX_error("Cannot do cmlAddNamespace after document output")

    call xml_DeclareNamespace(xf, URI, prefix)
  end subroutine cmlAddNamespace


  subroutine cmlStartCml(xf, id, title, convention, dictref, fileId, version)
    type(xmlf_t), intent(inout) :: xf

    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: fileId
    character(len=*), intent(in), optional :: version

    call xml_DeclareNamespace(xf, 'http://www.xml-cml.org/schema')
    call xml_DeclareNamespace(xf, 'http://www.w3.org/2001/XMLSchema', 'xsd')
    call xml_DeclareNamespace(xf, 'http://purl.org/dc/elements/1.1/title', 'dc')
    call xml_DeclareNamespace(xf, 'http://www.uszla.me.uk/FoX/units', 'units')
    call xml_DeclareNamespace(xf, 'http://www.xml-cml.org/units/units', 'cmlUnits')
    call xml_DeclareNamespace(xf, 'http://www.xml-cml.org/units/siUnits', 'siUnits')
    call xml_DeclareNamespace(xf, 'http://www.xml-cml.org/units/atomic', 'atomicUnits')
! FIXME TOHW we may want other namespaces in here - particularly for units
! once PMR has stabilized that.

    call xml_NewElement(xf, 'cml')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(convention)) then
      call xml_AddAttribute(xf, 'convention', convention)
    else
      call xml_AddAttribute(xf, 'convention', 'FoX_wcml-2.0')
    endif
    if (present(fileId)) then
      call xml_AddAttribute(xf, 'fileId', fileId)
    else
      call xml_AddAttribute(xf, 'fileId', xmlf_Name(xf))
    endif
    if (present(version)) then
      call xml_AddAttribute(xf, 'version', version)
    else
      call xml_AddAttribute(xf, 'version', "2.4")
    endif

  end subroutine cmlStartCml


  subroutine cmlEndCml(xf)
    type(xmlf_t), intent(inout) :: xf

    call cmlAddMetadata(xf, name='dc:contributor', content='FoX-'//FoX_version//' (http://www.uszla.me.uk/FoX)')
    call xml_EndElement(xf, 'cml')

  end subroutine cmlEndCml



end module m_wcml_core
