module m_dom_utils

  use m_common_array_str, only: str_vs, vs_str_alloc, vs_vs_alloc
  use m_common_attrs, only: dictionary_t, add_item_to_dict, getValue, &
    hasKey, init_dict, destroy_dict
  use m_common_format, only: operator(//)

  use m_dom_dom, only: Node, Namednodemap, Node
  use m_dom_dom, only: DOCUMENT_NODE, ELEMENT_NODE, TEXT_NODE, &
   CDATA_SECTION_NODE, COMMENT_NODE, DOCUMENT_TYPE_NODE, DOCUMENT_FRAGMENT_NODE, &
   ATTRIBUTE_NODE, ENTITY_NODE, NOTATION_NODE, ENTITY_REFERENCE_NODE, PROCESSING_INSTRUCTION_NODE
  use m_dom_dom, only: haschildnodes, getNodeName, getNodeType, &
    getFirstChild, getNextSibling, getlength, item, getDocumentElement, getOwnerElement, &
    getNameSpaceURI, getPrefix, getLocalName, getAttributes, getParentNode, &
    getNodeName, getNodeValue, getData, getName, getTagName, getValue, getTarget


  use FoX_wxml, only: xmlf_t
  use FoX_wxml, only: xml_OpenFile, xml_Close
  use FoX_wxml, only: xml_AddXMLDeclaration
  use FoX_wxml, only: xml_DeclareNamespace
  use FoX_wxml, only: xml_AddAttribute
  use FoX_wxml, only: xml_AddCharacters
  use FoX_wxml, only: xml_NewElement
  use FoX_wxml, only: xml_EndElement
  use FoX_wxml, only: xml_AddComment
  use FoX_wxml, only: xml_AddEntityReference
  use FoX_wxml, only: xml_AddXMLPI

  implicit none

  public :: dumpTree
  public :: serialize

  private

contains

  subroutine dumpTree(startNode)

    type(Node), pointer :: startNode   

    integer           :: indent_level

    indent_level = 0

    call dump2(startNode)

  contains

    recursive subroutine dump2(input)
      type(Node), pointer :: input
      type(Node), pointer :: temp     
      temp => input
      do while(associated(temp))
         write(*,'(3a,i0)') repeat(' ', indent_level), &
                        getNodeName(temp), " of type ", &
                        getNodeType(temp)
         write(*,'(2a)') 'containing value : ', getNodeValue(temp)
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

  subroutine serialize(startNode,Name)

    type(Node), pointer :: startNode   
    character(len=*), intent(in) :: Name

    type(xmlf_t)  :: xf
    integer :: iostat

    call xml_OpenFile(name, xf, iostat=iostat, unit=-1)
    if (iostat/=0) then
      print*,"IOSTAT", iostat
      stop
      continue
    endif
    call iter_dmp_xml(xf, startNode)
    call xml_Close(xf)

  end subroutine serialize

  subroutine iter_dmp_xml(xf, doc)
    type(xmlf_t), intent(inout) :: xf
    type(Node), pointer :: doc

    type(Node), pointer :: np
    integer :: i
    logical :: doneChildren, doneAttributes

    np => doc

    i = -1
    doneChildren = .false.
    doneAttributes = .false.
    print*,"STARTDUMP"
    do
      select case(getNodeType(np))
      case (ELEMENT_NODE)
        print*,"found an element", doneChildren, doneAttributes
        if (doneChildren) then
        print*,"close element ", getTagName(np)
          call xml_EndElement(xf, getTagName(np))
        elseif (.not.doneAttributes) then
          ! Are there any new prefixes or namespaces to be declared?
          ! FIXME
          print*,"open element ", getTagName(np)
          call xml_NewElement(xf, getTagName(np))
        endif
      case (ATTRIBUTE_NODE)
!        FIXME NODEVALUE WHEN ENTREF PRESENT
        print*,"AT"
        call xml_AddAttribute(xf, getName(np), getValue(np))
        doneChildren = .true. ! Ignore entity references for the moment.
      case (TEXT_NODE)
        print*,"TXT"
        call xml_AddCharacters(xf, getData(np))
      case (CDATA_SECTION_NODE)
        call xml_AddCharacters(xf, getData(np), parsed = .false.)
      case (ENTITY_REFERENCE_NODE)
        call xml_AddEntityReference(xf, getNodeName(np))
        doneChildren = .true. ! Do not serialize children, they are part of entity
      case (ENTITY_NODE)
        ! FIXME
        continue
      case (PROCESSING_INSTRUCTION_NODE)
        print*,"PI"
        call xml_AddXMLPI(xf, getTarget(np), getData(np))
      case (COMMENT_NODE)
        call xml_AddComment(xf, getData(np))
      case (DOCUMENT_NODE)
        print*,"DOC"
        continue
      case (DOCUMENT_TYPE_NODE)
        print*,"DT"
        ! FIXME
        continue
      case (DOCUMENT_FRAGMENT_NODE)
        continue
      case (NOTATION_NODE)
        ! FIXME
        continue
      end select

      if (getNodeType(np)==ELEMENT_NODE.and..not.doneAttributes) then
        if (getLength(getAttributes(np))>0) then
          np => item(getAttributes(np), 0)
        else
          doneAttributes = .true.
        endif
      elseif (getNodeType(np)==ATTRIBUTE_NODE) then
        i = i + 1
        if (i==getLength(getAttributes(getOwnerElement(np)))) then
          np => item(getAttributes(getOwnerElement(np)), i)
        else
          i = -1
          np => getOwnerElement(np)
          doneAttributes = .true.
        endif
      elseif (hasChildNodes(np).and..not.doneChildren) then
        np => getFirstChild(np)
        print*,"GETTINGFIRSTCHILD", getNodeType(np)
        doneChildren = .false.
        doneAttributes = .false.
      elseif (associated(getNextSibling(np))) then
        np => getNextSibling(np)
        print*,"GETTINGSIBLING", getNodeType(np)
        doneChildren = .false.
        doneAttributes = .false.
      else
        doneChildren = .true.
        np => getParentNode(np)
        if (associated(np, doc)) exit
      endif
    enddo

  end subroutine iter_dmp_xml
!!$  recursive subroutine dump_xml(xf, input)
!!$    type(xmlf_t)  :: xf
!!$    type(Node), pointer         :: input
!!$    type(Node), pointer         :: n, attr, child
!!$    type(NamedNodeMap), pointer :: attr_map
!!$    integer  ::  i
!!$    integer, save :: nns = -1
!!$    type(dictionary_t), save :: simpleDict
!!$    character, pointer :: prefix(:), elementQName(:)
!!$    character(len=100) :: abc
!!$    call init_dict(simpleDict)
!!$
!!$    n => input
!!$    if (.not. associated(n)) return
!!$    select case (getNodeType(n))
!!$
!!$    case (DOCUMENT_NODE)
!!$      call dump_xml(xf, getDocumentElement(n))
!!$      ! FIXME what about siblings of document element ...
!!$      
!!$    case (ELEMENT_NODE)
!!$      
!!$      if (len(getNamespaceURI(n))==0) then
!!$        elementQName => vs_str_alloc(getnodeName(n))
!!$        allocate(prefix(0))
!!$      else
!!$        if (hasKey(simpleDict, getNamespaceURI(n))) then
!!$          prefix => vs_str_alloc(getValue(simpleDict, getNamespaceURI(n)))
!!$        else
!!$          nns = nns + 1
!!$          if (nns == 0) then
!!$            allocate(prefix(0))
!!$          else
!!$            prefix => vs_str_alloc('ns'//nns)
!!$          endif
!!$          call add_item_to_dict(simpleDict, getNamespaceURI(n), getPrefix(n))
!!$        endif
!!$        if (size(prefix)==0) then
!!$          elementQName => vs_str_alloc(getNodeName(n))
!!$        else
!!$          elementQName => vs_str_alloc(getPrefix(n)//":"//getLocalName(n))
!!$        endif
!!$      endif
!!$      call xml_NewElement(xf, str_vs(elementQName))
!!$      if (len(getNamespaceURI(n))>0) then
!!$        if (size(prefix)==0) then
!!$          call xml_DeclareNamespace(xf, getNamespaceURI(n))
!!$        else
!!$          call xml_DeclareNamespace(xf, getNamespaceURI(n), getPrefix(n))
!!$        endif
!!$      endif
!!$      deallocate(prefix)
!!$      !TOHW need to check for & print out attribute namespaces as well.
!!$      attr_map => getAttributes(n)
!!$      do i = 0, getLength(attr_map) - 1
!!$        attr => item(attr_map,i)
!!$        call xml_AddAttribute(xf, getNodeName(attr), getNodeValue(attr))
!!$      enddo
!!$      child => getFirstChild(n)
!!$      do while (associated(child))
!!$        call dump_xml(xf, child)
!!$        child => getNextSibling(child)
!!$      enddo
!!$      call xml_EndElement(xf,str_vs(elementQName))
!!$      deallocate(elementQName)
!!$      
!!$    case (TEXT_NODE)
!!$      abc = getData(n)
!!$      print*,'THis data:'
!!$      do i = 1, len(getData(n))
!!$         print*,iachar(abc(i:i))
!!$      enddo
!!$      print*,'done'
!!$      
!!$      print*,'about to add chars'
!!$      call xml_AddCharacters(xf, getData(n))
!!$      print*,'done adding chars'
!!$    case (CDATA_SECTION_NODE)
!!$
!!$      call xml_AddCharacters(xf, getData(n), parsed=.false.)
!!$      
!!$    case (COMMENT_NODE)
!!$      
!!$      call xml_AddComment(xf, getData(n))
!!$      
!!$    end select
!!$    call destroy_dict(simpleDict)
!!$
!!$  end subroutine dump_xml
  
end module m_dom_utils
