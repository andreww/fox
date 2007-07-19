TOHW_m_dom_publics(`

! Assorted functions with identical signatures despite belonging to different types.

  public :: getData
  public :: setData
  public :: getName
  public :: getPublicId
  public :: getSystemId

')`'dnl
dnl
TOHW_m_dom_contents(`

  TOHW_m_dom_get(DOMString, data, np%nodeValue, (TEXT_NODE, COMMENT_NODE, CDATA_SECTION_NODE, PROCESSING_INSTRUCTION_NODE))

  TOHW_m_dom_set(DOMString, data, np%nodeValue, (TEXT_NODE, COMMENT_NODE, CDATA_SECTION_NODE, PROCESSING_INSTRUCTION_NODE))

  TOHW_m_dom_get(DOMString, name, np%nodeName, (DOCUMENT_TYPE_NODE, ATTRIBUTE_NODE))

  TOHW_m_dom_get(DOMString, publicId, np%dtdExtras%publicId, (DOCUMENT_TYPE_NODE, NOTATION_NODE, ENTITY_NODE))

  TOHW_m_dom_get(DOMString, systemId, np%dtdExtras%systemId, (DOCUMENT_TYPE_NODE, NOTATION_NODE, ENTITY_NODE))

')`'dnl
