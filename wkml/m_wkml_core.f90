module m_wkml_core

  use m_common_error, only: FoX_error
  use FoX_wxml, only: xmlf_t, xml_OpenFile, xml_Close, xml_DeclareNamespace, &
    xml_NewElement, xml_EndElement, xmlf_OpenTag

  use m_wkml_lowlevel, only: kmlOpenDocument, kmlCloseDocument

  private

  public :: kmlBeginFile
  public :: kmlFinishFile
  public :: kmlAddNamespace

contains

  subroutine kmlBeginFile(xf, filename, unit, replace, docName)
    type(xmlf_t), intent(out) :: xf
    character(len=*), intent(in) :: filename
    integer, intent(in) :: unit
    logical, intent(in), optional :: replace
    character(len=*), intent(in), optional :: docName

    if (unit==-1) then
      call xml_OpenFile(filename, xf, replace=replace)
    else
      call xml_OpenFile(filename, xf, unit=unit, replace=replace)
    endif
    call xml_DeclareNamespace(xf, 'http://earth.google.com/kml/2.2')
    call xml_NewElement(xf, 'kml')

    if (present(docName)) then
      call kmlOpenDocument(xf, docName)
    else
      call kmlOpenDocument(xf, 'WKML output')
    endif

  end subroutine kmlBeginFile


  subroutine kmlFinishFile(xf)
    type(xmlf_t), intent(inout) :: xf

    call kmlCloseDocument(xf)
    call xml_EndElement(xf, 'kml')
    call xml_Close(xf)

  end subroutine kmlFinishFile

  subroutine kmlAddNamespace(xf, prefix, URI)
    type(xmlf_t), intent(inout) :: xf

    character(len=*), intent(in) :: prefix
    character(len=*), intent(in) :: URI

    if (xmlf_OpenTag(xf) /= "") &
      call FoX_error("Cannot do kmlAddNamespace after document output")

    call xml_DeclareNamespace(xf, URI, prefix)
  end subroutine kmlAddNamespace

end module m_wkml_core
