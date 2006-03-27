module m_dom_namespaces

  use m_dom_types, only : fnode

  use m_dom_error, only : dom_error, NAMESPACE_ERR

  use m_strings, only : string, index, substr, len, assignment(=), operator(==), stringify

  implicit none
  private

  type URInodepair
    type(string) :: URI
    type(fNode), pointer :: node
  end type URInodepair

  type namespace
    type(string) :: prefix
    type(URInodepair), pointer :: unp(:)
  end type namespace

  type nsDictionary
    private
    type(namespace), pointer :: ns(:)
    integer :: dictLength = 0
  end type nsDictionary

  interface decomposeQName
    module procedure decomposeQName_s_vs_vs, decomposeQName_vs_vs_vs
  end interface

  interface currentNamespaceURI
    module procedure currentNamespaceURI_s, currentNamespaceURI_vs
  end interface

  public :: nsDictionary
  public :: nsPrefixAppend
  public :: nsClear
  public :: currentNamespaceURI
  public :: decomposeQName

contains

  subroutine nsPrefixAppend(nsDict, prefix, URI, node)
    type(nsDictionary), intent(inout) :: nsDict
    character(*), intent(in) :: prefix
    character(*), intent(in) :: URI
    type(fnode), pointer :: node

    type(namespace), allocatable :: tempns(:)
    type(URInodepair), allocatable :: tempunp(:)
    integer :: i, nsi, nsunp

    print*,'nsPrefixAppend: ', stringify(prefix)

    if (URI == '') then
      call dom_error('nsPrefixAppend', NAMESPACE_ERR, &
        'Tried to create empty namespace URI')
    endif

    nsi = nsDict%dictLength

    do i = 1, nsi
      if (nsDict%ns(i)%prefix == prefix) then
        ! We've already got this prefix. We must
        ! override it for all children of this node,
        ! but preserve the old values.
        ! Extend the URI & node arrays, and put
        ! the new values in the 1st element.
        nsunp = size(nsDict%ns(i)%unp)
        allocate(tempunp(nsunp))
        tempunp = nsDict%ns(i)%unp
        deallocate(nsDict%ns(i)%unp)
        nsunp = nsunp + 1
        allocate(nsDict%ns(i)%unp(nsunp))
        nsDict%ns(i)%unp(1:) = tempunp
        nsDict%ns(i)%unp(1)%URI = URI
        nsDict%ns(i)%unp(1)%node => node
        return
      endif
    enddo
    !Else we haven't got this prefix in the dictionary yet.
    print*,'new prefix'
    if (nsi == 0) then
      ! This is the first namespace we have.
      allocate(nsDict % ns(1))
      nsi = 1
    else
      !Extend dictionary ...
      allocate(tempns(nsi))
      tempns = nsDict%ns
      deallocate(nsDict%ns)
      nsi = nsi + 1
      allocate(nsDict%ns(nsi))
      nsDict%ns(:nsi-1) = tempns
    endif
    nsDict%ns(nsi)%prefix = prefix
    allocate(nsDict%ns(nsi)%unp(1))
    nsDict%ns(nsi)%unp(1)%URI = URI
    nsDict%ns(nsi)%unp(1)%node => node
    nsDict%dictLength = nsi

  end subroutine nsPrefixAppend


  subroutine nsClear(nsDict, node)
    type(nsDictionary), intent(inout) :: nsDict
    type(fNode), pointer :: node

    type(URInodepair), allocatable :: tempunp(:)
    integer :: i, nsi, nsunp

    print*,'nsClear: '

    nsi = nsDict%dictLength

    do i = 1, nsi
      !TOHW bug cannot do 1
      if (size(nsDict%ns(nsi)%unp) > 0) then
        if (associated(nsDict%ns(nsi)%unp(1)%node, node)) then
          ! A namespace of this prefix was added when
          ! this element opened. We must therefore clear
          ! it from the dictionary now.
          nsunp = size(nsDict%ns(nsi)%unp) - 1
          allocate(tempunp(nsunp))
          tempunp = nsDict%ns(nsi)%unp(:nsunp)
          deallocate(nsDict%ns(nsi)%unp)
          allocate(nsDict%ns(nsi)%unp(nsunp))
          nsDict%ns(nsi)%unp = tempunp
        endif
      endif
      ! We don't try to clear up empty prefix lists;
      ! more trouble than it's worth. Should only be
      ! referenced from currentNamespaceURI below anyway.
    enddo

  end subroutine nsClear


  function currentNamespaceURI_vs(nsDict, prefix) result(nsURI)
    type(nsDictionary), intent(in) :: nsDict
    type(string), intent(in) :: prefix
    type(string) :: nsURI

    integer :: i, nsi

    nsi = nsDict%dictLength
    nsURI = ''
    do i = 1, nsi
      if (nsDict%ns(i)%prefix == prefix) then
        if (size(nsDict%ns(i)%unp) > 0) then
          ! We need to check - prefix may have been
          ! created at some point but then deleted.
          nsURI = nsDict%ns(i)%unp(1)%URI
        endif
      endif
    enddo

  end function currentNamespaceURI_vs

  function currentNamespaceURI_s(nsDict, prefix) result(nsURI)
    type(nsDictionary), intent(in) :: nsDict
    character(len=*), intent(in) :: prefix
    type(string) :: nsURI

    integer :: i, nsi

    nsi = nsDict%dictLength
    nsURI = ''
    do i = 1, nsi
      if (nsDict%ns(i)%prefix == prefix) then
        if (size(nsDict%ns(i)%unp) > 0) then
          ! We need to check - prefix may have been
          ! created at some point but then deleted.
          nsURI = nsDict%ns(i)%unp(1)%URI
        endif
      endif
    enddo

  end function currentNamespaceURI_s

  subroutine decomposeQName_s_vs_vs(QName, prefix, localName)
    character(len=*), intent(in) :: QName
    type(string), intent(out) :: prefix, localName
    integer :: n

    !check is valid QName

    n = index(QName, ':')
    if (n > 0) then
      prefix = substr(QName, 1, n-1)
      localName = substr(QName, n+1, len(QName))
    else
      prefix = ''
      localName = QName
    endif

  end subroutine decomposeQName_s_vs_vs

  subroutine decomposeQName_vs_vs_vs(QName, prefix, localName)
    type(string), intent(in) :: QName
    type(string), intent(out) :: prefix, localName
    integer :: n

    !check is valid QName

    n = index(QName, ':')
    if (n > 0) then
      prefix = substr(QName, 1, n-1)
      localName = substr(QName, n+1, len(QName))
    else
      prefix = ''
      localName = QName
    endif

  end subroutine decomposeQName_vs_vs_vs

  function isValidQName(QName) result(p)
    type(string) :: QName
    logical :: p
    character(len=len(QName)) :: sQName

    sQName = QName
    !TOHW finish
    p = .true.
  end function isValidQName

end module m_dom_namespaces
