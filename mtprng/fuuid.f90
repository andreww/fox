module fuuid

  !This generates UUIDs according to RFC 4122
  
  use mtprng, only : mtprng_state, mtprng_init, mtprng_rand64
  implicit none
  
  integer, parameter :: i4b = selected_int_kind(9)
  integer, parameter :: i8b = selected_int_kind(18)
  
  character(len=*), parameter :: hexdigits = '0123456789abcdef'
  
  type(mtprng_state), save :: rng_state
  logical, save :: initialized = .false.
  integer, save :: values_save
  integer, save :: clock_seq = 0
  
contains
  
  function generate_uuid(version) result(uuid)
    integer, intent(in), optional :: version
    character(len=36) :: uuid

    integer(kind=i8b) :: timestamp, node
    integer(kind=i4b) :: clock_sequence

    integer(kind=i4b) :: time_low, time_mid, time_hi_and_version
    integer(kind=i4b) :: clk_seq_hi_res, clk_seq_low
    integer(kind=i4b) :: node01, node25
    integer(kind=i4b) :: octets(0:7)

    integer :: values(8)
    integer :: variant, v

    call date_and_time(values=values)
    if (all(values==values_save)) then
      clock_seq = clock_seq + 1
    else
      clock_seq = 0
    endif

    if (.not.initialized) then
      call mtprng_init(values(7)*10000+values(8), rng_state)
      initialized = .true.
    endif

    variant = 1
    if (present(version)) then
      v = version
    else
      v = 4
    endif

    select case (v)
    case (0)
      uuid = repeat('0',8)//'-'//repeat('0',4)//'-'//repeat('0',4)// &
        '-'//repeat('0',4)//'-'//repeat('0',12)
      return
    case default
      uuid = ''
    end select

!4.1.4 Timestamp

    select case(v)
    case(1)
      timestamp = get_utc_since_1582(values)
    case(2)
      !FIXME
    case(3)
      !FIXME md5 hash
    case(4)
      timestamp = mtprng_rand64(rng_state)
    case(5)
      !FIXME sha1 hash
    end select

!4.1.5 Clock Sequence
    ! 14 bits
    select case(v)
    case(1)
      clock_sequence = int(mtprng_rand64(rng_state), i4b)
    case(2)
      !FIXME
    case(3)
      !FIXME md5 hash see section 4.3
    case(4)
      clock_sequence = int(mtprng_rand64(rng_state), i4b)
    case(5)
      !FIXME sha1 hash see section 4.3
    end select

!4.1.6 Node
    ! 48 bits
    select case(v)
    case(1)
      node = mtprng_rand64(rng_state)
      ! No MAC address accessible - see section 4.5 !FIXME
    case(2)
      !FIXME
    case(3)
      !FIXME md5 hash see section 4.3
    case(4)
      node = mtprng_rand64(rng_state)
    case(5)
      !FIXME sha1 hash see section 4.3
    end select

    timestamp = ishft(timestamp, 4)
    time_low = ibits(timestamp, 0, 32)
    time_mid = int(ibits(timestamp, 32, 16), i4b)
    time_hi_and_version = iand(ishft(int(ibits(timestamp, 48, 12), i4b), 4), v)
    
    clk_seq_hi_res = iand(ishft(ibits(clock_sequence, 0, 8), 2), variant)

    clk_seq_low = ibits(clock_sequence, 8, 8)

    node01 = int(ibits(node, 0, 16), i4b)
    node25 = int(ibits(node, 16, 32), i4b)

    octets = 0
    octets(0) = ibits(time_low, 0, 16)
    octets(1) = ibits(time_low, 16, 16)
    octets(2) = time_mid
    octets(3) = time_hi_and_version
    octets(4) = iand(ishft(clk_seq_hi_res, 8), clk_seq_low)
    octets(5) = node01
    octets(6) = ibits(node25, 0, 16)
    octets(7) = ibits(node25, 16, 16)

    uuid = intToHexOctet(octets(0))//intToHexOctet(octets(1))//"-"// &
      intToHexOctet(octets(2))//"-"//intToHexOctet(octets(3))//"-"// &
      intToHexOctet(octets(4))//"-"//intToHexOctet(octets(5))// &
      intToHexOctet(octets(6))//intToHexOctet(octets(7))

  contains

    function intToHexOctet(b) result(s)
      integer, intent(inout) :: b
      character(len=4) :: s
      
      integer :: i
      
      do i = 1, 4
        s(i:i) = hexdigits(ibits(b, 0, 4):ibits(b, 0, 4))
        b = ishft(b, 4)
      enddo
      
    end function intToHexOctet
    
  end function generate_uuid
  

  function get_utc_since_1582(values) result(ns)
    ! This subroutine is a little broken. It takes no account
    ! of any future leapseconds. It ought to serve regardless.

    ! It returns the number of 100-ns intervals since 1582-10-15-00-00-00

    ! It only works for times after December 2005. But that shouldn't matter.
    integer, dimension(8), intent(in) :: values
    integer(kind=i8b) :: ns

    integer :: days
    integer :: years

    integer, parameter :: days_in_normal_year(12) = &
      (/31, 28, 31, 30, 31, 30, 31, 31, 30, 30, 30, 31/)

    ns = 23 * 1000 * 10 ! 23 leap seconds until 31/12/2005

    ! A count of the 100-nanosecond intervals since the
    ! beginning of the day.
    ns = ns + 10 * values(8) &
      + 1000 * 10 * values(7) &
      + 60 * 1000 *10 * (values(6) - values(4)) &
      + 60 * 60 * 1000 * 10 * values(5)

    ! Number of days this year:
    days = sum(days_in_normal_year(:values(2)))
    if (values(2)>2 .and. isLeapYear(values(1))) then
      days = days + 1
    endif
    days = days + 81 ! From the 15/10-31/12 last year

    years = values(1) - 1583
    days = days + values(3) !find days in current year:
    !Add extra leap days to this total:
    days = days + years/4
    !And subtract off un-leap-days.
    if (years < (1800 - 1582)) days = days - 1
    if (years < (1900 - 1582)) days = days - 2
    if (years < (1981 - 1582)) days = days - 3

    ns = ns + 864000000 * (days + 365 * years)

  contains
    function isLeapYear(y) result(p)
      integer, intent(in) :: y
      logical :: p
      p = .false. !FIXME
    end function isLeapYear

  end function get_utc_since_1582


end module fuuid
