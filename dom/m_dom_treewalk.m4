define(`TOHW_m_dom_treewalk',`dnl
dnl Walk a DOM tree, including attributes & their children
dnl
dnl Every node will be hit twice, once on the way down, once
dnl on the way back up.
dnl Except element nodes which will be hit an additional time
dnl after attributes are done before children are done
dnl
dnl First argument is what to do when doneChildren = .false.
dnl Second argument is what to do when doneChildren = .true.
dnl
dnl This requires declarations of:
dnl integer :: i
dnl logical :: doneChildren, doneAttributes
dnl
dnl The root of the tree to be walked must be pointed to by the 
dnl variable "treeroot"
dnl The primary node being walked must be called "this"
dnl In addition, for cloneNode/importNode, a secondary node
dnl may be walked which represents its parent for a cloned tree
dnl That must be called "thatParent"
dnl This can be switched on with $3 = thatParent
dnl For destroyNode, another node can be tracked which represents the
dnl last node hit which we can delete. It will be called "deadNode",
dnl and will be destroyed as soon as it is finished with.
dnl That can be switched on with $3 = deadNode
dnl

    i_tree = 0
    doneChildren = .false.
    doneAttributes = .false.
    this => treeroot
ifelse(`$3', `deadNode',`
      deadNode => null()
')dnl
    do

      if (.not.doneChildren) then
$1
      else
        if (getNodeType(this)==ELEMENT_NODE) doneAttributes = .true.
$2
      endif

ifelse(`$3', `deadNode',`
      deadNode => null()
')dnl
      if (.not.doneChildren) then

        if (getNodeType(this)==ELEMENT_NODE.and..not.doneAttributes) then
          if (getLength(getAttributes(this))>0) then
          ifelse(`$3', `parentNode', `dnl
            if (.not.associated(this, arg)) thatParent => getLastChild(thatParent)
')dnl
            this => item(getAttributes(this), 0)
          else
ifelse(`$3', `parentNode', `dnl
            if (.not.deep) exit
')dnl
            doneAttributes = .true.
          endif
        elseif (hasChildNodes(this)) then
ifelse(`$3', `parentNode', `dnl
          if (.not.associated(this, arg)) then
            if (getNodeType(this)==ATTRIBUTE_NODE) then
              thatParent => item(getAttributes(thatParent), i_tree)
            else
              thatParent => getLastChild(thatParent)
            endif
          endif
')dnl
          this => getFirstChild(this)
          doneChildren = .false.
          doneAttributes = .false.
        else
          doneChildren = .true.
          doneAttributes = .false.
        endif

      else ! if doneChildren

ifelse(`$3', `deadNode', `dnl
        deadNode => this
')dnl
        if (associated(this, arg)) exit
        if (getNodeType(this)==ATTRIBUTE_NODE) then
          if (i_tree<getLength(getAttributes(getOwnerElement(this)))-1) then
            i_tree= i_tree+ 1
            this => item(getAttributes(getOwnerElement(this)), i_tree)
            doneChildren = .false.
          else
            i_tree= 0
ifelse(`$3', `parentNode', `dnl
            if (associated(getParentNode(thatParent))) thatParent => getParentNode(thatParent)
')dnl
            this => getOwnerElement(this)
            doneAttributes = .true.
            doneChildren = .false.
          endif
        elseif (associated(getNextSibling(this))) then

          this => getNextSibling(this)
          doneChildren = .false.
          doneAttributes = .false.
        else
          this => getParentNode(this)
ifelse(`$3', `parentNode', `dnl
          if (.not.associated(this, arg)) then
            if (getNodeType(this)==ATTRIBUTE_NODE) then
              thatParent => getOwnerElement(thatParent)
            else
              thatParent => getParentNode(thatParent)
            endif
          endif
')dnl
        endif
ifelse(`$3', `deadNode',`
        call destroy(deadNode)
')dnl
      endif

    enddo

')
