define(`TOHWM4_declarationtype', `dnl
ifelse(`$1', `RealDp', `real(dp)', 
       `$1', `RealSp', `real(sp)', 
       `$1', `Int', `integer', 
       `$1', `Lg', `logical', 
       `$1', `Ch', `character(len=*)') dnl
')dnl
dnl
define(`TOHWM4_metadata_sub',`dnl
   subroutine cmlAddMetadata$1(xf, name, content, convention, dictRef, id, title dnl
ifelse(substr($1,0,4),`Real',`,fmt)',`)')
    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in) :: name
    TOHWM4_declarationtype($1), intent(in) :: content
    character(len=*), intent(in), optional :: convention
    character(len=*), intent(in), optional :: dictRef
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title

ifelse(substr($1,0,4),`Real',`dnl 
    character(len=*), intent(in), optional :: fmt
')dnl

    call xml_NewElement(xf, "metadata")
    call xml_AddAttribute(xf, "name", name)
    call xml_AddAttribute(xf, name="content", value=content dnl
ifelse(substr($1,0,4),`Real',`,fmt=fmt',`') dnl
)
    if (present(dictref)) call xml_AddAttribute(xf, "dictRef", dictref)
    if (present(id)) call xml_AddAttribute(xf, "id", title)
    if (present(title)) call xml_AddAttribute(xf, "title", title)
    if (present(convention)) call xml_AddAttribute(xf, "convention", convention)
    call xml_EndElement(xf, "metadata")

   end subroutine cmlAddMetadata$1
')dnl
dnl
dnl
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

TOHWM4_metadata_sub(`Lg')

TOHWM4_metadata_sub(`Int')

TOHWM4_metadata_sub(`RealSp')

TOHWM4_metadata_sub(`RealDp')

end module m_wcml_metadata
