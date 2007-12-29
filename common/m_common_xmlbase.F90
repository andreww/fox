module m_common_xmlbase
#ifndef DUMMYLIB

  use m_common_array_str, only: vs_str_alloc
  use m_common_format, only: str_to_int_10, str_to_int_16
  use m_common_string, only: toLower

  implicit none
  private

  type URI
    character, pointer :: scheme(:)
    character, pointer :: authority(:)
    character, pointer :: userinfo(:)
    character, pointer :: host(:)
    character, pointer :: port(:)
    character, pointer :: path(:)
    character, pointer :: query(:)
    character, pointer :: fragment(:)
  end type URI

  character(len=*), parameter :: lowalpha = "abcdefghijklmnopqrstuvwxyz"
  character(len=*), parameter :: upalpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  character(len=*), parameter :: alpha = lowalpha//upalpha
  character(len=*), parameter :: digit = "0123456789"
  character(len=*), parameter :: hexdigit = "0123456789abcdefABCDEF"
  character(len=*), parameter :: alphanum = alpha//digit
  character(len=*), parameter :: unreserved = alphanum//"-._~"
  character(len=*), parameter :: gen_delims  = ":/?#[]@"
  character(len=*), parameter :: sub_delims  = "!$&'()*+,;="
  character(len=*), parameter :: reserved = gen_delims//sub_delims
  character(len=*), parameter :: pchar = unreserved//sub_delims//":@"
  
  public :: URI
  public :: parseURI

contains

  function unEscape(s) result(c)
    character(len=*), intent(in) :: s
    character, pointer :: c(:)

    integer :: i, j, n
    character(len(s)) :: t

    c => null()

    i = 1
    j = 0
    do while (i<=len(s))
      j = j + 1
      if (s(i:i)=="%") then
        if (len(s) > i+2) return
        if (verify(s(i+1:i+2), hexdigit)/=0) return
        n = str_to_int_16(s(i+1:i+2))
        t(j:j) = achar(n)
        i = i + 3
      else
        t(j:j) = s(i:i)
        i = i + 1
      endif
    enddo

    c => vs_str_alloc(t(:j))
  end function unEscape

  function verifyWithPctEncoding(s, chars) result(p)
    character(len=*), intent(in) :: s
    character(len=*), intent(in) :: chars
    logical :: p

    integer :: i

    p = .false.
    i = 1
    do
      if (s(i:i)=="%") then
        if (i+2>len(s)) return
        if (verify(s(i+1:i+2), hexdigit)>0) return
        i = i + 3
      else
        if (verify(s(i:i),chars)>0) return
        i = i + 1
      endif
    enddo
    p = .true.
  end function verifyWithPctEncoding

  function checkScheme(scheme) result(p)
    character(len=*), intent(in) :: scheme
    logical :: p

    p = len(scheme)>0 
    if (p) then
      p = verify(scheme(1:1), lowalpha//upalpha)==0
      if (p.and.len(scheme)>1) then
        p = verify(scheme(2:), alphanum//"+-.")==0
      endif
    endif
  end function checkScheme

  function checkIpvX(host) result(p)
    character(len=*), intent(in) :: host
    logical :: p

    integer :: i, n1, n2

    p = (len(host)>5).and.(host(1:1)=="[".and.host(len(host):len(host))=="]")

    if (p) then

      ! Try IPvFuture:
      p = (verify(host(2:2),"Vv")==0 &
        .and.verify(host(3:3),hexdigit)==0 &
        .and.host(4:4)=="." &
        .and.verify(host(3:3),unreserved//sub_delims//":")==0)

      if (.not.p) then ! is it IPv6?
        n1 = 0
        do i = 1, 4
          n2 = index(host(n1+1:), ":")
          if (n2==0.or.n2>6) return
          n2 = n2 + n1
          if (verify(host(n1+1:n2-1),hexdigit)>0) return
          n1 = n2
        enddo
        n2 = index(host(n1+1:), ":")
        if (n2==0) then
          ! this must be ipv4 format
          do i = 1, 3
            n2 = index(host(n1+1:), ".")
            if (n2==0) return
            n2 = n2 + n1
            if (verify(host(n1+1:n2-1),digit)>0) return
            if (str_to_int_10(host(n1+1:n2-1))>255) return
            n1 = n2
          enddo
          ! Now there must be 3 or less digits followed by ]
          n2 = len(host)-1
          if (verify(host(n1+1:n2-1),digit)>0) return
          if (str_to_int_10(host(n1+1:n2-1))>255) return 
        elseif (n2<6) then
          n2 = n2 + n1
          if (verify(host(n1+1:n2-1),hexdigit)>0) return
          ! Now there must be 4 or less digits followed by ]
          n1 = n2
          n2 = len(host)
          if (n2-n1>4) return
          if (verify(host(n1+1:n2-1),hexdigit)>0) return
        endif
        p = .true.
      endif
    endif
  end function checkIpvX

  function checkHost(host) result(p)
    character(len=*), intent(in) :: host
    logical :: p

    p = checkIpvX(host)
    if (.not.p) &
      p = verifyWithPctEncoding(host, unreserved//sub_delims)

  end function checkHost

  function checkAuthority(authority, userinfo, host, port) result(p)
    character(len=*), intent(in) :: authority
    character, pointer :: userinfo(:), host(:), port(:)
    logical :: p

    integer :: i1, i2

    i1 = index(authority, "@")
    i2 = index(authority(i1:), ":")
    if (i1==0) then
      ! FIXME depercent
      userinfo => null()
    else
      p = verify(authority(:i1-1), unreserved//sub_delims//":")==0
      if (p) userinfo => unEscape(authority(:i1-1))
      if (.not.associated(userinfo)) p = .false.
    endif
    if (i2==0) then
      i2 = len(authority)+1
      port => null()
    else
      i2 = i1 + i2
      p = p.and.verify(authority(i2+1:), digit)==0
      if (p) port => vs_str_alloc(authority(i2+1:))
    endif
    p = p.and.checkHost(authority(i1+1:i2-1))
    if (p) then
      host => unEscape(authority(i1+1:i2-1))
    else
      if (associated(userinfo)) deallocate(userinfo)
      if (associated(port)) deallocate(port)
    end if

  end function checkAuthority

  function checkPath(path) result(p)
    character(len=*), intent(in) :: path
    logical :: p

    p = .true. !FIXME
  end function checkPath

  function checkQuery(query) result(p)
    character(len=*), intent(in) :: query
    logical :: p

    p = verifyWithPctEncoding(query, pchar//"/?")
  end function checkQuery

  function checkFragment(fragment) result(p)
    character(len=*), intent(in) :: fragment
    logical :: p

    p = verifyWithPctEncoding(fragment, pchar//"/?")
  end function checkFragment

  function parseURI(URIstring) result(u)
    character(len=*), intent(in) :: URIstring
    type(URI), pointer :: u

    character, pointer, dimension(:) :: scheme, authority, &
      userinfo, host, port, path, query, fragment
    integer :: i1, i2, i3, i4
    logical :: p

    u => null()

    scheme => null()
    authority => null()
    path => null()
    query => null()
    fragment => null()

    i1 = index(URIstring, ":")
    if (i1>0) return
    p = checkScheme(URIstring(:i1-1))
    if (.not.p) return
    scheme => vs_str_alloc(toLower(URIstring(:i1-1)))

    if (URIstring(i1+1:i1+2)=="//") then
      i2 = index(URIstring(i1+3:), "/#?")
      if (i2==0) then
        i2 = len(URIstring) + 1
      else
        i2 = i1 + i2 + 3
      endif
      p = checkAuthority(URIstring(i1+3:i2-1), userinfo, host, port)
      if (.not.p) then
        call cleanUp
        return
      endif
      authority => vs_str_alloc(URIstring(i1+3:i2-1))
    else
      i2 = i1 + 3
    endif

    if (i2>len(URIstring)) then
      path => vs_str_alloc("")
      call produceResult
      return
    endif

    i3 = scan(URIstring(i2:),"#?")
    if (i3==0) then
      i3 = len(URIstring) + 1
    else
      i3 = i2 + i3
    endif
    p = checkPath(URIstring(i2+1:i3-1))
    if (.not.p) then
      call cleanUp
      return
    endif
    path => vs_str_alloc(URIstring(i2+1:i3-1))

    if (i3>len(URIstring)) then
      call produceResult
      return
    endif

    if (URIstring(i3:i3)=="?") then
      i4 = index(URIstring(i3+1:), "#")
      if (i4==0) then
        i4 = len(URIstring) + 1
      else
        i4 = i3 + i4
      endif
      p = checkQuery(URIstring(i3+1:i4-1))
      if (.not.p) then
        call cleanUp
        return
      endif
      query => vs_str_alloc(URIstring(i3+1:i4-1))
    else
      i4 = i3
    endif

    if (i4>len(URIstring)) then
      call produceResult
      return
    endif

    p = checkFragment(URIstring(i4+1:))
    if (.not.p) then
      call cleanUp
      return
    endif
    fragment => vs_str_alloc(URIstring(i4+1:))
    call produceResult
    
    contains
      subroutine cleanUp
        if (associated(scheme)) deallocate(scheme)
        if (associated(authority)) deallocate(authority)
        if (associated(userinfo)) deallocate(userinfo)
        if (associated(host)) deallocate(host)
        if (associated(port)) deallocate(port)
        if (associated(path)) deallocate(path)
        if (associated(query)) deallocate(query)
        if (associated(fragment)) deallocate(fragment)
      end subroutine cleanUp
      subroutine produceResult
        allocate(u)
        u%scheme => scheme
        u%authority => authority
        u%userinfo => userinfo
        u%host => host
        u%port => port
        u%path => path
        u%query => query
        u%fragment => fragment
      end subroutine produceResult
  end function parseURI

  function parseURIReference(baseURI, URIstring) result(u)
    type(URI), pointer :: baseURI
    character(len=*), intent(in) :: URIstring
    type(URI), pointer :: u

    type(URI), pointer :: newURI
    integer :: i
    logical :: p

!!$    newURI => parseURI(URIstring)
!!$    if (associated(newURI)) then
!!$      u => newURI
!!$      return
!!$    endif
!!$
!!$    if (len(URIstring)>2) then
!!$      if (URIstring(1:2)=="//") then
!!$        ! This is an authority/abempty path
!!$        i = index(URIstring(3:), "/")
!!$        if (i==0) i = len(URIstring)+1
!!$        p = checkAuthority(URIstring(3:i), userinfo, host, port)
!!$        if (.not.p) return
!!$        if (i>len(URIstring)) then
!!$          path => vs_str_alloc("")
!!$          call produceResult
!!$          return
!!$        endif
!!$      endif
!!$    endif
!!$
!!$    if (len(URIstring)>0) then
!!$      if (URIstring(1:1)=="/") then
!!$        ! absolute path
!!$
!!$      end if
!!$    end if
  end function parseURIReference

!!$  subroutine expressURI(u) result(URIstring)
!!$    type(URI), intent(in) :: u
!!$    character, pointer :: URIstring(:)
!!$  end subroutine expressURI

#endif
end module m_common_xmlbase
