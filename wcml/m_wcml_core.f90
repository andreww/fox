module m_wcml_core

  use FoX_common, only: FoX_version
  use m_common_format, only: str
  use m_common_realtypes, only: sp, dp
  use FoX_wxml, only: xmlf_t, xml_OpenFile, xml_Close
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement, xml_DeclareNamespace

  implicit none
  private

  public :: cmlBeginFile
  public :: cmlFinishFile
  public :: cmlAddNamespace
  
  public :: cmlStartCml
  public :: cmlEndCml

  public :: cmlStartModule
  public :: cmlEndModule

  public :: cmlStartStep
  public :: cmlEndStep

contains

  subroutine cmlBeginFile(xf, filename)
    type(xmlf_t), intent(out) :: xf
    character(len=*), intent(in) :: filename

    call xml_OpenFile(filename, xf)

  end subroutine cmlBeginFile

  
  subroutine cmlFinishFile(xf)
    type(xmlf_t), intent(out) :: xf

    call xml_Close(xf)

  end subroutine cmlFinishFile


  subroutine cmlAddNamespace(xf, prefix, URI)
    type(xmlf_t), intent(inout) :: xf
    
    character(len=*), intent(in) :: prefix
    character(len=*), intent(in) :: URI

    !FIXME
    !if (len(xf%stack) > 0) &
    !  call FoX_error("Cannot do cmlAddNamespace after document output")

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
    call xml_DeclareNamespace(xf, 'http://www.xml-cml.org/units/units', 'cmlUnits')
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
! FIXME
!    if (present(fileId)) then
!      call xml_AddAttribute(xf, 'fileId', fileId)
!    else
!      call xml_AddAttribute(xf, 'fileId', str_vs(xf%filename))
!    endif
    if (present(version)) then
      call xml_AddAttribute(xf, 'version', version)
    else
      call xml_AddAttribute(xf, 'version', "2.4")
    endif

  end subroutine cmlStartCml


  subroutine cmlEndCml(xf)
    type(xmlf_t), intent(inout) :: xf

  !  call cmlAddMetadata(xf, name='dc:contributor', content='FoX-'//FoX_version//' (http://www.eminerals.org)')
    call xml_EndElement(xf, 'cml')

  end subroutine cmlEndCml


  subroutine cmlStartModule(xf, id, title, convention, dictref, role, serial)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: role
    character(len=*), intent(in), optional :: serial
    
    call xml_NewElement(xf, 'module')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(convention)) call xml_AddAttribute(xf, 'convention', convention)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    if (present(serial)) call xml_AddAttribute(xf, 'serial', serial)
    
  end subroutine cmlStartModule


  subroutine cmlEndModule(xf)
    type(xmlf_t), intent(inout) :: xf

    call xml_EndElement(xf, 'module')
    
  end subroutine cmlEndModule


  subroutine cmlStartStep(xf, type, index, id, title, convention)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: type
    character(len=*), intent(in), optional :: id
    integer, intent(in), optional :: index
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: convention

    if (present(index)) then
      call cmlStartModule(xf=xf, id=id, title=title, convention=convention, &
        dictRef=type, role='step', serial=str(index))
    else
      call cmlStartModule(xf=xf, id=id, title=title, convention=convention, &
        dictRef=type, role='step')
    endif
    
  end subroutine cmlStartStep


  subroutine cmlEndStep(xf)
    type(xmlf_t), intent(inout) :: xf

    call xml_EndElement(xf, 'module')
    
  end subroutine cmlEndStep


end module m_wcml_core
