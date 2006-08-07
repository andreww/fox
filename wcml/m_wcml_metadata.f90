module m_wcml_metadata

  use m_common_realtypes, only: sp, dp
  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_NewElement, xml_EndElement
  use FoX_wxml, only: xml_AddAttribute

  implicit none
  private

  interface cmlAddMetadata
    module procedure cmlAddMetaDataCh
    module procedure cmlAddMetaDataInt
    module procedure cmlAddMetaDataLg
    module procedure cmlAddMetaDataRealSP
    module procedure cmlAddMetaDataRealDP
  end interface

  public :: cmlStartMetadataList
  public :: cmlEndMetadataList
  public :: cmlAddMetadata

contains

  subroutine cmlStartMetadataList(xf, id, title, convention, dictref, name, role)

    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: name
    character(len=*), intent(in), optional :: role

    call xml_NewElement(xf, 'metadataList')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(convention)) call xml_AddAttribute(xf, 'convention', convention)
    if (present(name)) call xml_AddAttribute(xf, 'name', name)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)

  end subroutine cmlStartMetadataList

  subroutine cmlEndMetadataList(xf)

    type(xmlf_t), intent(inout) :: xf
    call xml_EndElement(xf, 'metadataList')

  end subroutine cmlEndMetadataList

  subroutine cmlAddMetadataCh(xf, name, content, convention, dictRef, id, title)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    character(len=*), intent(in) :: content
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title

    call xml_NewElement(xf, 'metadata')
    call xml_AddAttribute(xf, 'name', name)
    call xml_AddAttribute(xf, 'content', content)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(id)) call xml_AddAttribute(xf, 'id', title)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(convention)) call xml_AddAttribute(xf, 'convention', convention)
    call xml_EndElement(xf, 'metadata')

  end subroutine cmlAddMetadataCh

   subroutine cmlAddMetadataLg(xf, name, content, convention, dictRef, id, title )
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    logical , intent(in) :: content
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title


    call xml_NewElement(xf, "metadata")
    call xml_AddAttribute(xf, "name", name)
    call xml_AddAttribute(xf, name="content", value=content  )
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictref)
    if (present(id)) call xml_AddAttribute(xf, "id", title)
    if (present(title)) call xml_AddAttribute(xf, "title", title)
    if (present(convention)) call xml_AddAttribute(xf, "convention", convention)
    call xml_EndElement(xf, "metadata")

   end subroutine cmlAddMetadataLg


   subroutine cmlAddMetadataInt(xf, name, content, convention, dictRef, id, title )
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    integer , intent(in) :: content
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title


    call xml_NewElement(xf, "metadata")
    call xml_AddAttribute(xf, "name", name)
    call xml_AddAttribute(xf, name="content", value=content  )
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictref)
    if (present(id)) call xml_AddAttribute(xf, "id", title)
    if (present(title)) call xml_AddAttribute(xf, "title", title)
    if (present(convention)) call xml_AddAttribute(xf, "convention", convention)
    call xml_EndElement(xf, "metadata")

   end subroutine cmlAddMetadataInt


   subroutine cmlAddMetadataRealSp(xf, name, content, convention, dictRef, id, title ,fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    real(sp) , intent(in) :: content
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title

    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, "metadata")
    call xml_AddAttribute(xf, "name", name)
    call xml_AddAttribute(xf, name="content", value=content ,fmt=fmt )
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictref)
    if (present(id)) call xml_AddAttribute(xf, "id", title)
    if (present(title)) call xml_AddAttribute(xf, "title", title)
    if (present(convention)) call xml_AddAttribute(xf, "convention", convention)
    call xml_EndElement(xf, "metadata")

   end subroutine cmlAddMetadataRealSp


   subroutine cmlAddMetadataRealDp(xf, name, content, convention, dictRef, id, title ,fmt)
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    real(dp) , intent(in) :: content
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title

    character(len=*), intent(in), optional :: fmt

    call xml_NewElement(xf, "metadata")
    call xml_AddAttribute(xf, "name", name)
    call xml_AddAttribute(xf, name="content", value=content ,fmt=fmt )
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictref)
    if (present(id)) call xml_AddAttribute(xf, "id", title)
    if (present(title)) call xml_AddAttribute(xf, "title", title)
    if (present(convention)) call xml_AddAttribute(xf, "convention", convention)
    call xml_EndElement(xf, "metadata")

   end subroutine cmlAddMetadataRealDp


end module m_wcml_metadata
