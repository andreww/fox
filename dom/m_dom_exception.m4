include(`foreach.m4')`'dnl
include(`m_dom_treewalk.m4')`'dnl
define(`TOHW_m_dom_throw_error',`dnl
dnl 1 is numerical code
dnl 2 is list of things to deallocate
call throw_exception($1, "m4f_thisfunc", ex)
if (present(ex)) then
  if (inException(ex)) then
ifelse($2, `', `', 
     m4_foreach(`x', `$2', `
if (associated(x)) deallocate(x)
'))`'dnl
     return
  endif
endif
')`'dnl
dnl
define(`TOHW_function', `define(`m4f_thisfunc', `$1')`'function $1`'dnl
(m4_foreach(`x', `$2', `x, ')ex)ifelse($3, `', `', `result($3)') 
    type(DOMException), intent(out), optional :: ex`'dnl
')`'dnl
dnl
define(`TOHW_subroutine', `define(`m4f_thisfunc', `$1')`'subroutine $1`'dnl
(m4_foreach(`x', `$2', `x, ')ex)
    type(DOMException), intent(out), optional :: ex`'dnl
')`'dnl
dnl
define(`TOHW_m_dom_hierarchy_test',`dnl
      select case(testParent%nodeType)
      case (ELEMENT_NODE)
        if (testChild%nodeType/=ELEMENT_NODE &
          .and. testChild%nodeType/=TEXT_NODE &
          .and. testChild%nodeType/=COMMENT_NODE &
          .and. testChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
          .and. testChild%nodeType/=CDATA_SECTION_NODE &
          .and. testChild%nodeType/=ENTITY_REFERENCE_NODE) then
          TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
        endif
      case (ATTRIBUTE_NODE)
        if (testChild%nodeType/=TEXT_NODE &
          .and. testChild%nodeType/=ENTITY_REFERENCE_NODE) then
          TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
        endif
        if (testChild%nodeType==ENTITY_REFERENCE_NODE) then
          treeroot => testChild
TOHW_m_dom_treewalk(`
          if (getNodeType(this)/=TEXT_NODE.and.getNodeType(this)/=ENTITY_REFERENCE_NODE) then
            TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
          endif
',`')
        endif
      case (DOCUMENT_NODE)
        if (testChild%nodeType/=ELEMENT_NODE &
          .and. testChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
          .and. testChild%nodeType/=COMMENT_NODE &
          .and. testChild%nodeType/=DOCUMENT_TYPE_NODE)  then
          TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
        endif
      case (DOCUMENT_FRAGMENT_NODE)
        if (testChild%nodeType/=ELEMENT_NODE &
          .and. testChild%nodeType/=TEXT_NODE &
          .and. testChild%nodeType/=COMMENT_NODE &
          .and. testChild%nodeType/=PROCESSING_INSTRUCTION_NODE &
          .and. testChild%nodeType/=CDATA_SECTION_NODE &
          .and. testChild%nodeType/=ENTITY_REFERENCE_NODE) then
          TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
        endif
      case (ENTITY_NODE)
        continue ! only allowed by DOM parser, not by user.
        ! but entity nodes are always readonly anyway, so no problem
      case (ENTITY_REFERENCE_NODE)
        continue ! only allowed by DOM parser, not by user.
        ! but entity nodes are always readonly anyway, so no problem
      case default
        TOHW_m_dom_throw_error(HIERARCHY_REQUEST_ERR)
      end select
')
