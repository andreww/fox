module FoX_wxml

  use m_wxml_text
  use m_wxml_core
  use m_wxml_overloads

  implicit none
  private

  public :: str

  public :: xmlf_t
  public :: xml_OpenFile
  public :: xml_Close
  public :: xml_NewElement
  public :: xml_EndElement
  public :: xml_AddXMLDeclaration
  public :: xml_AddXMLStylesheet
  public :: xml_AddXMLPI
  public :: xml_AddComment
  public :: xml_AddTextSection

  public :: xml_AddPcdata
  public :: xml_AddAttribute
  public :: xml_AddPseudoAttribute

  public :: xml_AddNamespace
  public :: xml_addDOCTYPE

end module FoX_wxml
