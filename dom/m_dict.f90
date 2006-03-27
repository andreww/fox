module m_dict

  use m_strings, only : string, operator(==), assignment(=), stringify

  implicit none
  private

  type dict_item
    private
    type(string) :: key
    type(string) :: value
  end type dict_item

  type dictionary
    private
    type(dict_item), pointer :: dict(:) => null()
  end type dictionary

  interface addKey
    module procedure addKey
  end interface addKey

  interface hasKey
    module procedure hasKey
  end interface hasKey

  interface getValue
    module procedure getValue
  end interface getValue

  interface removeKey
    module procedure removeKey
  end interface removeKey

  public :: dictionary
  public :: addKey
  public :: hasKey
  public :: getValue
  public :: removeKey

  contains

    subroutine addKey(dict, key, value)
      type(dictionary), intent(inout) :: dict
      type(string), intent(in) :: key
      type(string), intent(in) :: value

      type(dict_item), pointer :: tempdict(:)

      integer :: i, n


      if (associated(dict%dict)) then
        n = 0
        do i = 1, size(dict%dict)
          if (dict%dict(i)%key == key) then
            n = i
            exit
          endif
        enddo
        if (n==0) then
          n = size(dict%dict)
          allocate(tempdict(n))
          tempdict = dict%dict
          deallocate(dict%dict)
          allocate(dict%dict(n+1))
          dict%dict(:n) = tempdict
          n = n+1
        endif
      else
        allocate(dict%dict(1))
        n = 1
      endif
      dict%dict(n)%key = key
      dict%dict(n)%value = value

    end subroutine addKey

    function hasKey(dict, key)
      type(dictionary), intent(in) :: dict
      type(string), intent(in) :: key
      logical :: hasKey

      integer :: i
      
      hasKey = .false.
      if (associated(dict%dict)) then
        do i = 1, size(dict%dict)
          if (dict%dict(i)%key == key) then
            hasKey = .true.
            exit
          endif
        enddo
      endif

    end function hasKey

    function getValue(dict, key)
      type(dictionary), intent(in) ::dict
      type(string), intent(in) :: key
      type(string) :: getValue

      integer :: i
      
      getValue = ''
      if (associated(dict%dict)) then
        do i = 1, size(dict%dict)
          if (dict%dict(i)%key == key) then
            getValue = dict%dict(i)%value
            exit
          endif
        enddo
      endif

    end function getValue

    subroutine removeKey(dict, key)
      type(dictionary), intent(inout) :: dict
      type(string), intent(in) :: key

      type(dict_item), pointer :: tempdict(:)
      integer :: i, m, n

      if (associated(dict%dict)) then
        m = 0
        do i = 1, size(dict%dict)
          if (dict%dict(i)%key == key) then
            m = i
            exit
          endif
        enddo
        if (m/=0) then
          n = size(dict%dict)
          allocate(tempdict(n-1))
          tempdict(:m) = dict%dict(:m)
          tempdict(m:) = dict%dict(m+1:)
          deallocate(dict%dict)
          allocate(dict%dict(n-1))
          dict%dict = tempdict
        endif
      end if

    end subroutine removeKey
      

  end module m_dict
