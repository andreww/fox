module m_sax_namespaces

  use m_common_array_str, only : compare_array_str
  use FoX_common, only : dictionary_t, get_key, get_value, remove_key, len
  use FoX_common, only : set_nsURI, set_localName
  use m_common_error, only : FoX_error

  implicit none
  private

  character(len=*), parameter :: invalidNS = '::INVALID::'
  ! an invalid URI name to indicate a namespace error.

  type URIMapping
    character, dimension(:), pointer :: URI
    integer :: ix ! link back to node depth
  end type URIMapping
  !This is a single URI, and the node depth under which
  !its namespace applies.

  type prefixMapping
    character, dimension(:), pointer :: prefix
    type(URIMapping), dimension(:), pointer :: urilist
  end type prefixMapping
  !This is the mapping for a single prefix; with the
  !list of namespaces which are in force at various
  !depths

  type namespaceDictionary
    private
    type(URIMapping), dimension(:), pointer :: defaults
    type(prefixMapping), dimension(:), pointer :: prefixes
  end type namespaceDictionary
  !This is the full namespace dictionary; defaults is
  !the list of default namespaces in force; prefix a
  !list of all prefixes in force.

  public :: invalidNS

  public :: initNamespaceDictionary
  public :: destroyNamespaceDictionary
  public :: namespaceDictionary
  public :: checkNamespaces
  public :: checkEndNamespaces
  public :: getnamespaceURI
  interface getnamespaceURI
     module procedure getURIofDefaultNS, getURIofPrefixedNS
  end interface

contains

  subroutine checkNamespaces(atts, nsDict, ix)
    type(dictionary_t), intent(inout) :: atts
    type(namespaceDictionary), intent(inout) :: nsDict
    integer, intent(in) :: ix ! depth of nesting of current element.

    character(len=6) :: xmlns
    character, dimension(:), allocatable :: prefix, QName, URI
    integer :: i, j, n, xmlnsLength, URIlength

    !Check for namespaces; *and* remove xmlns references from 
    !the attributes dictionary.

    ! we can't do a simple loop across the attributes,
    ! because we need to remove some as we go along ...
    i = 1
    do while (i <= len(atts))
       xmlns = get_key(atts, i)
       if (xmlns == 'xmlns ') then
          !Default namespace is being set
          URIlength = len(get_value(atts, i))
          allocate(URI(URIlength))
          URI = transfer(get_value(atts, i), URI)
          call checkURI(URI)
          call addDefaultNS(nsDict, URI, ix)
          deallocate(URI)
          call remove_key(atts, i)
          !TOHW call startNamespaceMapping
       elseif (xmlns == 'xmlns:') then
          !Prefixed namespace is being set
          URIlength = len(get_value(atts, i))
          allocate(URI(URIlength))
          URI = transfer(get_value(atts, i), URI)
          call checkURI(URI)
          xmlnsLength = len(get_key(atts, i))
          allocate(QName(xmlnsLength))
          allocate(prefix(xmlnsLength - 6))
          QName = transfer(get_key(atts, i), QName)
          prefix = QName(7:)
          call addPrefixedNS(nsDict, prefix, URI, ix)
          !TOHW call startNamespaceMapping
          deallocate(QName)
          deallocate(prefix)
          call remove_key(atts, i)
       else
          ! we only uncrement if we haven't removed a key
          i = i+1
       endif
    enddo

    ! having done that, now resolve all attribute namespaces:
    do i = 1, len(atts)
       ! get name
       allocate(QName(len(get_key(atts, i))))
       QName = transfer(get_key(atts,i), QName)
       n = 0
       do j = 1, size(QName)
          if (QName(j) == ':') then
             n = j
             exit
          endif
       enddo
       if (n > 0) then
          allocate(prefix(n-1))
          prefix = transfer(QName(1:n-1), prefix)
          print*,'nsURI',getnamespaceURI(nsDict, prefix)
          call set_nsURI(atts, i, getnamespaceURI(nsDict, prefix))
          deallocate(prefix)
       else
          call set_nsURI(atts, i, '') ! no such thing as a default namespace on attributes
       endif
       call set_localName(atts, i, QName(n+1:))
       deallocate(QName)
    enddo

  end subroutine checkNamespaces


  subroutine initNamespaceDictionary(nsDict)
    type(namespaceDictionary), intent(out) :: nsDict

    !We need to properly initialize 0th elements
    !(which are never used) in order to provide
    !sensible behaviour when trying to retrieve
    !from an empty dictionary.

    allocate(nsDict%defaults(0:0))
    allocate(nsDict%defaults(0)%URI(0))
    !The 0th element of the defaults NS is the empty namespace
    
    allocate(nsDict%prefixes(0:0))
    allocate(nsDict%prefixes(0)%prefix(0))
    allocate(nsDict%prefixes(0)%urilist(0:0))
    allocate(nsDict%prefixes(0)%urilist(0)%URI(len(invalidNS)))
    nsDict%prefixes(0)%urilist(0)%URI = &
         transfer(invalidNS, nsDict%prefixes(0)%urilist(0)%URI)
    
    
  end subroutine initNamespaceDictionary


  subroutine destroyNamespaceDictionary(nsDict)
    type(namespaceDictionary), intent(inout) :: nsDict

    integer :: i, j
    do i = 0, ubound(nsDict%defaults,1)
       deallocate(nsDict%defaults(i)%URI)
    enddo
    deallocate(nsDict%defaults)
    do i = 0, ubound(nsDict%prefixes,1)
       do j = 0, ubound(nsDict%prefixes(i)%urilist,1)
          deallocate(nsDict%prefixes(i)%urilist(j)%URI)
       enddo
       deallocate(nsDict%prefixes(i)%urilist)
    enddo
    deallocate(nsDict%prefixes(0)%prefix)
    deallocate(nsDict%prefixes)
  end subroutine destroyNamespaceDictionary


  subroutine copyURIMapping(urilist1, urilist2, l_m)
    type(URIMapping), dimension(0:), intent(inout) :: urilist1
    type(URIMapping), dimension(0:), intent(inout) :: urilist2
    integer, intent(in):: l_m
    integer :: i

    if (ubound(urilist1,1) < l_m .or. ubound(urilist2,1) < l_m) then
       call FoX_error('Internal error in m_sax_namespaces:copyURIMapping')
    endif
    ! Now copy all defaults across ...
    do i = 0, l_m
       urilist2(i)%ix = urilist1(i)%ix
       urilist2(i)%URI => urilist1(i)%URI
       !and deallocate all current defaults
       nullify(urilist1(i)%URI)
    enddo

  end subroutine copyURIMapping
    
  
  subroutine addDefaultNS(nsDict, uri, ix)
    type(namespaceDictionary), intent(inout) :: nsDict
    character, dimension(:), intent(in) :: uri
    integer, intent(in) :: ix

    type(URIMapping), dimension(:), allocatable :: tempMap
    integer :: l_m, l_s

    l_m = ubound(nsDict%defaults,1)
    allocate(tempMap(0:l_m))
    ! Now copy all defaults across ...
    call copyURIMapping(nsDict%defaults, tempMap, l_m)
    deallocate(nsDict%defaults)
    l_m = l_m + 1
    allocate(nsDict%defaults(0:l_m))
    !Now copy everything back ...
    call copyURIMapping(tempMap, nsDict%defaults, l_m-1)
    deallocate(tempMap)
    ! And finally, add the new default NS
    nsDict%defaults(l_m)%ix = ix
    l_s = size(uri)
    allocate(nsDict%defaults(l_m)%URI(l_s))
    nsDict%defaults(l_m)%URI = uri

  end subroutine addDefaultNS
  

  subroutine addPrefixedURI(nsPrefix, uri, ix)
    type(PrefixMapping), intent(inout) :: nsPrefix
    character, dimension(:), intent(in) :: uri
    integer, intent(in) :: ix

    type(URIMapping), dimension(:), allocatable :: tempMap
    integer :: l_m, l_s

    l_m = ubound(nsPrefix%urilist,1)
    allocate(tempMap(0:l_m))
    ! Now copy all dacross ...
    call copyURIMapping(nsPrefix%urilist, tempMap, l_m)
    deallocate(nsPrefix%urilist)
    l_m = l_m + 1
    allocate(nsPrefix%urilist(0:l_m))
    !Now copy everything back ...
    call copyURIMapping(tempMap, nsPrefix%urilist, l_m-1)
    deallocate(tempMap)
    ! And finally, add the new default NS
    nsPrefix%urilist(l_m)%ix = ix
    l_s = size(uri)
    allocate(nsPrefix%urilist(l_m)%URI(l_s))
    nsPrefix%urilist(l_m)%URI = uri

  end subroutine addPrefixedURI
   
  subroutine removeDefaultNS(nsDict)
    type(namespaceDictionary), intent(inout) :: nsDict

    type(URIMapping), dimension(:), allocatable :: tempMap
    integer :: l_m

    l_m = ubound(nsDict%defaults,1)
    allocate(tempMap(0:l_m-1))
    ! Now copy all defaults across ...
    call copyURIMapping(nsDict%defaults, tempMap, l_m-1)
    !And remove tail-end charlie
    deallocate(nsDict%defaults(l_m)%URI)
    deallocate(nsDict%defaults)
    l_m = l_m - 1
    allocate(nsDict%defaults(0:l_m))
    !Now copy everything back ...
    call copyURIMapping(tempMap, nsDict%defaults, l_m)
    deallocate(tempMap)

  end subroutine removeDefaultNS

  subroutine removePrefixedURI(nsPrefix)
    type(PrefixMapping), intent(inout) :: nsPrefix

    type(URIMapping), dimension(:), allocatable :: tempMap
    integer :: l_m

    l_m = ubound(nsPrefix%urilist,1)
    allocate(tempMap(0:l_m-1))
    ! Now copy all defaults across ...
    call copyURIMapping(nsPrefix%urilist, tempMap, l_m-1)
    !And remove tail-end charlie
    deallocate(nsPrefix%urilist(l_m)%URI)
    deallocate(nsPrefix%urilist)
    l_m = l_m - 1
    allocate(nsPrefix%urilist(0:l_m))
    !Now copy everything back ...
    call copyURIMapping(tempMap, nsPrefix%urilist, l_m)
    deallocate(tempMap)

  end subroutine removePrefixedURI

  subroutine addPrefixedNS(nsDict, prefix, URI, ix)
    type(namespaceDictionary), intent(inout) :: nsDict
    character, dimension(:), intent(in) :: prefix
    character, dimension(:), intent(in) :: uri
    integer, intent(in) :: ix
    
    integer :: l_p, p_i, i
    l_p = ubound(nsDict%prefixes, 1)
    
    p_i = 0
    do i = 1, l_p
       if (compare_array_str(nsDict%prefixes(l_p)%prefix, prefix)) then
          p_i = i
          exit
       endif
    enddo

    if (p_i == 0) then
       call addPrefix(nsDict, prefix)
       p_i = l_p + 1
    endif

    call addPrefixedURI(nsDict%prefixes(p_i), URI, ix)
    
  end subroutine addPrefixedNS

  subroutine removePrefixedNS(nsDict, prefix)
    type(namespaceDictionary), intent(inout) :: nsDict
    character, dimension(:), intent(in) :: prefix
    integer :: l_p, p_i, i
    l_p = ubound(nsDict%prefixes, 1)
    
    p_i = 0
    do i = 1, l_p
       if (compare_array_str(nsDict%prefixes(l_p)%prefix, prefix)) then
          p_i = i
          exit
       endif
    enddo

    if (p_i /= 0) then
       call removePrefixedURI(nsDict%prefixes(p_i))
       if (ubound(nsDict%prefixes(p_i)%urilist,1) == 0) then
          !that was the last mapping for that prefix
          call removePrefix(nsDict, p_i)
       endif
    else
       call FoX_error('Internal error in m_sax_namespaces:removePrefixedNS')
    endif
    
  end subroutine removePrefixedNS

  subroutine addPrefix(nsDict, prefix)
    type(namespaceDictionary), intent(inout) :: nsDict
    character, dimension(:), intent(in) :: prefix
    integer :: l_p

    type(prefixMapping), dimension(:), pointer :: tempPrefixMap

    integer :: i

    !Add a new prefix to the namespace dictionary.
    !Unfortunately this involves copying the entire
    !prefixes dictionary to a temporary structure, then
    !reallocating the prefixes dictionary to be one
    !longer, then copying everything back:

    l_p = ubound(nsDict%prefixes, 1)
    allocate(tempPrefixMap(0:l_p))

    !for each current prefix, append everything to temporary structure
    do i = 0, l_p
       tempPrefixMap(i)%prefix => nsDict%prefixes(i)%prefix
       tempPrefixMap(i)%urilist => nsDict%prefixes(i)%urilist
    enddo
    deallocate(nsDict%prefixes)
    !extend prefix dictionary by one ...
    l_p = l_p + 1
    allocate(nsDict%prefixes(0:l_p))
    !and copy back ...
    do i = 0, l_p-1
       nsDict%prefixes(i)%prefix => tempPrefixMap(i)%prefix
       nsDict%prefixes(i)%urilist => tempPrefixMap(i)%urilist
    enddo
    deallocate(tempPrefixMap)

    allocate(nsDict%prefixes(l_p)%prefix(size(prefix)))
    nsDict%prefixes(l_p)%prefix = prefix
    allocate(nsDict%prefixes(l_p)%urilist(0:0))
    allocate(nsDict%prefixes(l_p)%urilist(0)%URI(0))
    
  end subroutine addPrefix

  subroutine removePrefix(nsDict, i_p)
    type(namespaceDictionary), intent(inout) :: nsDict
    integer, intent(in) :: i_p
    integer :: l_p

    type(prefixMapping), dimension(:), pointer :: tempPrefixMap

    integer :: i

    !Remove a prefix from the namespace dictionary.
    !Unfortunately this involves copying the entire
    !prefixes dictionary to a temporary structure, then
    !reallocating the prefixes dictionary to be one
    !shorter, then copying everything back:

    l_p = ubound(nsDict%prefixes, 1)
    allocate(tempPrefixMap(0:l_p-1))

    !for each current prefix, append everything to temporary structure
    do i = 0, i_p-1
       tempPrefixMap(i)%prefix => nsDict%prefixes(i)%prefix
       tempPrefixMap(i)%urilist => nsDict%prefixes(i)%urilist
    enddo
    deallocate(nsDict%prefixes(i_p)%urilist(0)%URI)
    deallocate(nsDict%prefixes(i_p)%urilist)
    deallocate(nsDict%prefixes(i_p)%prefix)
    !this subroutine will only get called if the urilist is already
    !empty, so no need to deallocate it.
    do i = i_p+1, l_p
       tempPrefixMap(i-1)%prefix => nsDict%prefixes(i)%prefix
       tempPrefixMap(i-1)%urilist => nsDict%prefixes(i)%urilist
    enddo
    deallocate(nsDict%prefixes)
    !shorten prefix dictionary by one ...
    l_p = l_p - 1
    allocate(nsDict%prefixes(0:l_p))
    !and copy back ...
    do i = 0, l_p
       nsDict%prefixes(i)%prefix => tempPrefixMap(i)%prefix
       nsDict%prefixes(i)%urilist => tempPrefixMap(i)%urilist
    enddo
    deallocate(tempPrefixMap)

  end subroutine removePrefix


  subroutine checkEndNamespaces(nsDict, ix)
    type(namespaceDictionary), intent(inout) :: nsDict
    integer, intent(in) :: ix

    integer :: l_d, l_p, l_ps, i

    !It will only ever be the final element in the list that
    ! might have expired.
    l_d = ubound(nsDict%defaults,1)
    if (nsDict%defaults(l_d)%ix == ix) then
       call removeDefaultNS(nsDict)
       !TOHWFIXME call endPrefixMapping
    endif

    l_p = ubound(nsDict%prefixes, 1)
    do i = 1, l_p
       l_ps = ubound(nsDict%prefixes(l_p)%urilist,1)
       if (nsDict%prefixes(l_p)%urilist(l_ps)%ix == ix) then
          call removePrefixedNS(nsDict, nsDict%prefixes(i)%prefix)
          !TOHWFIXME call endPrefixMapping
       endif
    enddo

  end subroutine checkEndNamespaces

  subroutine dumpnsdict(nsdict)
    type(namespaceDictionary), intent(in) :: nsdict
    integer :: i, j
    print*,'* default namespaces *'

    do i = 1, ubound(nsdict%defaults, 1)
       print*, nsdict%defaults(i)%ix, nsdict%defaults(i)%URI
    enddo
    print*,'* Prefixed namespaces *'
    do i = 1, ubound(nsdict%prefixes, 1)
       print*,'* prefix: ', nsdict%prefixes(i)%prefix
       do j = 1, ubound(nsdict%prefixes(i)%urilist, 1)
          print*, nsdict%prefixes(i)%urilist(j)%ix, nsdict%prefixes(i)%urilist(j)%URI
       enddo
    enddo
    !call pxfflush(6)
  end subroutine dumpnsdict

  pure function getURIofDefaultNS(nsDict) result(uri)
    type(namespaceDictionary), intent(in) :: nsDict
    character(len=size(nsDict%defaults(ubound(nsDict%defaults,1))%URI)) :: URI
    
    integer :: l_d
    l_d = ubound(nsDict%defaults,1)
    uri = transfer(nsDict%defaults(l_d)%URI, uri)
  end function getURIofDefaultNS

  pure function isPrefixInForce(nsDict, prefix) result(force)
    type(namespaceDictionary), intent(in) :: nsDict
    character, dimension(:), intent(in) :: prefix
    logical :: force
    integer :: i

    force = .false.
    do i = 1, ubound(nsDict%prefixes, 1)
       if (compare_array_str(nsDict%prefixes(i)%prefix, prefix)) &
            force = .true.
    enddo

  end function isPrefixInForce

  pure function getPrefixIndex(nsDict, prefix) result(p)
    type(namespaceDictionary), intent(in) :: nsDict
    character, dimension(:), intent(in) :: prefix
    integer :: p
    
    integer :: i
    p = 0
    do i = 1, ubound(nsDict%prefixes, 1)
       if (compare_array_str(nsDict%prefixes(i)%prefix, prefix)) then
          p = i
          exit
       endif
    enddo
  end function getPrefixIndex

  pure function getURIofPrefixedNS(nsDict, prefix) result(uri)
    type(namespaceDictionary), intent(in) :: nsDict
    character, dimension(:), intent(in) :: prefix
    character(len=size( &
              nsDict%prefixes( &
           getPrefixIndex(nsDict,prefix) &
                             ) &
                     %urilist( &
           ubound(nsDict%prefixes(getPrefixIndex(nsDict,prefix))%urilist, 1) &
                             ) & 
                      %uri)) :: URI
    integer :: p_i, l_m
    p_i = getPrefixIndex(nsDict, prefix)
    l_m = ubound(nsDict%prefixes(p_i)%urilist, 1)
    uri = transfer(nsDict%prefixes(p_i)%urilist(l_m)%URI,uri)

  end function getURIofPrefixedNS


  subroutine checkURI(URI)
    character, dimension(:) :: URI

    !FIXMETOHW check that the string is a valid URI
    !TOHW actually, namespaces 1.1 says just a valid QName

  end subroutine checkURI


end module m_sax_namespaces
