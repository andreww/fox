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
dnl The primary node being walked must be called "this"
dnl In addition, for cloneNode/importNode, a secondary node
dnl may be walked which represents its parent for a cloned tree
dnl That must be called "thatParent"
dnl This can be switched on with $3 = thatParent
dnl For destroyNode, another node can be tracked which represents the
dnl last node hit which we can delete. It will be called "deadNode"
dnl That can be switched on with $4 = deadNode
dnl

    i = 0
    doneChildren = .false.
    doneAttributes = .false.
    do

      if (.not.doneChildren) then

$1

      else

$2

      endif

      if (.not.doneChildren) then

        if (getNodeType(this)==ELEMENT_NODE.and..not.doneAttributes) then
          if (getLength(getAttributes(this))>0) then
            if (.not.associated(this, arg)) thatParent => getLastChild(thatParent)
            this => item(getAttributes(this), 0)
          else
            if (.not.deep) return
            doneAttributes = .true.
          endif
        elseif (hasChildNodes(this)) then
          if (.not.associated(this, arg)) then
            if (getNodeType(this)==ATTRIBUTE_NODE) then
              thatParent => item(getAttributes(thatParent), i)
            else
              thatParent => getLastChild(thatParent)
            endif
          endif
          this => getFirstChild(this)
          doneChildren = .false.
          doneAttributes = .false.
        else
          doneChildren = .true.
        endif

      else ! if doneChildren

        if (associated(this, arg)) exit
        if (getNodeType(this)==ATTRIBUTE_NODE) then
          if (i<getLength(getAttributes(getOwnerElement(this)))-1) then
            i = i + 1
            this => item(getAttributes(getOwnerElement(this)), i)
            doneChildren = .false.
          else
            i = 0
            if (associated(getParentNode(thatParent))) thatParent => getParentNode(thatParent)
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
          if (.not.associated(this, arg)) then
            if (getNodeType(this)==ATTRIBUTE_NODE) then
              thatParent => getOwnerElement(thatParent)
            else
              thatParent => getParentNode(thatParent)
            endif
          endif
        endif

      endif

    enddo

')
