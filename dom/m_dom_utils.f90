module m_dom_utils

  use m_dom_types, only: fnode, fnamednodemap, fDocumentNode
  use m_dom_types, only: element_node
  use m_dom_types, only: text_node, cdata_section_node
  use m_dom_types, only: comment_node

  use m_dom_node, only: getnodetype, getnodename
  use m_dom_node, only: getnodevalue, getfirstchild
  use m_dom_node, only: getnextsibling
  use m_dom_node, only: haschildnodes, getattributes
  use m_dom_namednodemap, only: getlength, item

  use m_strings, only: string, stringify, operator(==), operator(/=), assignment(=), operator(+)
  use m_dict, only : dictionary, addKey, getValue, hasKey

  use xmlf90_wxml, only: xmlf_t
  use xmlf90_wxml, only: xml_OpenFile, xml_Close
  use xmlf90_wxml, only: xml_AddXMLDeclaration
  use xmlf90_wxml, only: xml_AddAttribute
  use xmlf90_wxml, only: xml_AddPcdata
  use xmlf90_wxml, only: xml_NewElement
  use xmlf90_wxml, only: xml_EndElement
  use xmlf90_wxml, only: xml_AddCdataSection
  use xmlf90_wxml, only: xml_AddComment

  implicit none

  public :: dumpTree
  public :: serialize

  interface serialize
    module procedure serialize_node
    module procedure serialize_doc_node
  end interface

  private

CONTAINS

  subroutine dumpTree(startNode)

    type(fnode), pointer :: startNode   

    character(len=50) :: indent = " "
    integer           :: indent_level
    type(string)      :: s

    indent_level = 0

    call dump2(startNode)

  contains

    recursive subroutine dump2(input)
      type(fnode), pointer :: input
      type(fnode), pointer :: temp     
      temp => input
      do while(associated(temp))
         s = getNodeName(temp)
         write(*,'(3a,i3)') indent(1:indent_level), &
                        stringify(s), " of type ", &
                        getNodeType(temp)
         if (hasChildNodes(temp)) then
            indent_level = indent_level + 3
            call dump2(getFirstChild(temp))
            indent_level = indent_level - 3
         endif
         temp => getNextSibling(temp)
      enddo

    end subroutine dump2

  end subroutine dumpTree
!----------------------------------------------------------------

  subroutine serialize_node(startNode,fname)

    type(fnode), pointer :: startNode   
    character(len=*), intent(in) :: fname

    type(xmlf_t)  :: xf

    call xml_OpenFile(fname,xf)
    call dump_xml(xf, startNode)
    call xml_Close(xf)

  end subroutine serialize_node

  subroutine serialize_doc_node(startNode, fname)
    type(fDocumentNode), pointer :: startNode   
    character(len=*), intent(in) :: fname

    type(xmlf_t)  :: xf

    call xml_OpenFile(fname,xf)
    call xml_AddXMLDeclaration(xf)
    call dump_xml(xf, startNode%documentElement)
    call xml_Close(xf)
  end subroutine serialize_doc_node

  recursive subroutine dump_xml(xf, input)
    type(xmlf_t)  :: xf
    type(fnode), pointer         :: input
    type(fnode), pointer         :: node, attr, child
    type(fnamedNodeMap), pointer :: attr_map
    integer  ::  i
    integer, save :: nns = -1
    type(string) :: prefix, nsURI
    type(string)  :: s, sv, sn
    type(dictionary), save :: simpleDict

    node => input
    if (.not. associated(node)) return
    select case (getNodeType(node))
      
    case (ELEMENT_NODE)
      
      nsURI = node % namespaceURI
      print*,'nsURI ', nns, ' ',stringify(nsURI)
      if (nsURI == '') then
        s = getNodeName(node)
      else
        if (hasKey(simpleDict, nsURI)) then
          prefix = getValue(simpleDict, nsURI)
        else
          nns = nns + 1
          if (nns == 0) then
            prefix = ''
          else
            prefix = 'ns' + stringify(nns)
          endif
          print*,'adding key: ',  nns, stringify(nsURI), stringify(prefix)
          call addKey(simpleDict,nsURI,prefix)
        endif
        if (prefix == '') then
          s = node%localName
        else
          s = prefix + ':' + node % localName
        endif
      endif
      print*,'aa ', s%s(:)
      print*,'bb ', stringify(s)
      call xml_NewElement(xf,stringify(s))
      if (nsURI /= '') then
        if (prefix == '') then
          call xml_AddAttribute(xf, 'xmlns', stringify(nsURI))
        else
          call xml_AddAttribute(xf, stringify('xmlns:'+prefix), stringify(nsURI))
        endif
      endif
      !TOHW need to check for & print out attribute namespaces as well.
      attr_map => getAttributes(node)
      do i = 0, getLength(attr_map) - 1
        attr => item(attr_map,i)
        sn = getNodeName(attr)
        sv = getNodeValue(attr)
        call xml_AddAttribute(xf, stringify(sn), stringify(sv))
      enddo
      child => node % firstChild
      do while (associated(child))
        call dump_xml(xf, child)
        child => child % nextSibling
      enddo
      call xml_EndElement(xf,stringify(s))
      
    case (TEXT_NODE)
      
      s = getNodeValue(node)
      call xml_AddPcdata(xf,stringify(s))
      
    case (CDATA_SECTION_NODE)
      
      s = getNodeValue(node)
      call xml_AddCdataSection(xf,stringify(s))
      
    case (COMMENT_NODE)
      
      s = getNodeValue(node)
      call xml_AddComment(xf,stringify(s))
      
    end select

  end subroutine dump_xml
  
end module m_dom_utils
