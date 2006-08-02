module m_wcml_core

  use FoX_common, only: FoX_version
  use m_common_format, only: str
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_AddAttribute
  use FoX_wxml, only: xml_EndElement, xml_DeclareNamespace

  implicit none
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)
  
  public :: cmlStartCml
  public :: cmlEndCml

  public :: cmlStartModule
  public :: cmlEndModule

  public :: cmlStartStep
  public :: cmlEndStep

contains

  subroutine cmlStartCml(xf, id, title, conv, dictref, ref)
    type(xmlf_t), intent(inout) :: xf

    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref

    call xml_DeclareNamespace(xf, 'http://www.xml-cml.org/schema')
    call xml_DeclareNamespace(xf, 'http://www.w3.org/2001/XMLSchema', 'xsd')
    call xml_DeclareNamespace(xf, 'http://purl.org/dc/elements/1.1/title', 'dc')
! FIXME TOHW we will want other namespaces in here - particularly for units
! once PMR has stabilized that.

    call xml_NewElement(xf, 'cml')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)

  end subroutine cmlStartCml


  subroutine cmlEndCml(xf)
    type(xmlf_t), intent(inout) :: xf

  !  call cmlAddMetadata(xf, name='dc:contributor', content='FoX-'//FoX_version//' (http://www.eminerals.org)')
    call xml_EndElement(xf, 'cml')

  end subroutine cmlEndCml


  subroutine cmlStartModule(xf, id, title, conv, dictref, ref, role, serial)
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
    
  end subroutine cmlStartModule


  subroutine cmlEndModule(xf)
    type(xmlf_t), intent(inout) :: xf

    call xml_EndElement(xf, 'module')
    
  end subroutine cmlEndModule


  subroutine cmlStartStep(xf, type, index, id, title, conv, ref)
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
    
  end subroutine cmlStartStep


  subroutine cmlEndStep(xf)
    type(xmlf_t), intent(inout) :: xf

    call xml_EndElement(xf, 'module')
    
  end subroutine cmlEndStep


end module m_wcml_core
