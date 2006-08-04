module m_wcml_metadata

  use m_common_realtypes, only: sp, dp
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_EndElement
  use FoX_wxml, only: xml_AddAttribute

  implicit none
  private

  interface cmlAddMetadata
    module procedure cmlAddMetaDataCh
    module procedure cmlAddMetaDataI
    module procedure cmlAddMetaDataSP
    module procedure cmlAddMetaDataDP
    module procedure cmlAddMetaDataLg
  end interface

  public :: cmlStartMetadataList
  public :: cmlEndMetadataList
  public :: cmlAddMetadata

contains

  subroutine cmlStartMetadataList(xf, id, title, conv, dictref, ref, role)

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

  end subroutine cmlStartMetadataList

  subroutine cmlEndMetadataList(xf)

    type(xmlf_t), intent(inout) :: xf
    call xml_EndElement(xf, 'metadataList')

  end subroutine cmlEndMetadataList

  subroutine cmlAddMetadataCh(xf, name, content, conv, dictRef, id, title)
!dictRef, id, title
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

  subroutine cmlAddMetadataSP(xf, name, content, conv)

    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    real(sp), intent(in) :: content
    character(len=*), intent(in), optional :: conv

    call xml_NewElement(xf, 'metadata')
    call xml_AddAttribute(xf, 'name', name)
    call xml_AddAttribute(xf, 'content', content)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    call xml_EndElement(xf, 'metadata')

  end subroutine cmlAddMetadataSP

  subroutine cmlAddMetadataDP(xf, name, content, conv)

    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    real(dp), intent(in) :: content
    character(len=*), intent(in), optional :: conv

    call xml_NewElement(xf, 'metadata')
    call xml_AddAttribute(xf, 'name', name)
    call xml_AddAttribute(xf, 'content', content)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    call xml_EndElement(xf, 'metadata')

  end subroutine cmlAddMetadataDP

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

end module m_wcml_metadata
