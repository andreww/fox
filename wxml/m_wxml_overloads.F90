module m_wxml_overloads
  
  use m_wxml_text, only: str
  use m_wxml_core, only: xmlf_t
  use m_wxml_core, only: xml_AddPcData_Ch => xml_AddPcData
  use m_wxml_core, only: xml_AddAttribute_Ch => xml_AddAttribute
  use m_wxml_core, only: xml_AddPseudoAttribute_Ch => xml_AddPseudoAttribute

  implicit none
  private

  integer, parameter ::  sp = selected_real_kind(6,30)
  integer, parameter ::  dp = selected_real_kind(14,100)

  interface xml_AddPcData
     module procedure xml_AddPcdata_SP
     module procedure xml_AddPcdata_DP
     module procedure xml_AddPcdata_Int
     module procedure xml_AddPcdata_Log
  end interface

  interface xml_AddAttribute
     module procedure xml_AddAttribute_SP
     module procedure xml_AddAttribute_DP
     module procedure xml_AddAttribute_Int
     module procedure xml_AddAttribute_Log
  end interface

  interface xml_AddPseudoAttribute
     module procedure xml_AddPseudoAttribute_SP
     module procedure xml_AddPseudoAttribute_DP
     module procedure xml_AddPseudoAttribute_Int
     module procedure xml_AddPseudoAttribute_Log
  end interface

  public :: xml_AddPcdata
  public :: xml_AddAttribute
  public :: xml_AddPseudoAttribute

contains

  !-------------------------------------------------------------------

  subroutine xml_AddPcdata_SP(xf,pcdata,fmt, space,line_feed)
    type(xmlf_t), intent(inout)    :: xf
    real(kind=sp), intent(in)      :: pcdata
    logical,          intent(in), optional  :: space
    logical,          intent(in), optional  :: line_feed
    character(len=*), intent(in), optional  :: fmt

#ifdef PGF90
    if (present(fmt)) then
      call xml_AddPcdata_Ch(xf,trim(str(pcdata,fmt)),space,line_feed)
    else
      call xml_AddPcdata_Ch(xf,trim(str(pcdata)),space,line_feed)
    endif
#else
    if (present(fmt)) then
      call xml_AddPcdata_Ch(xf,str(pcdata,fmt),space,line_feed)
    else
      call xml_AddPcdata_Ch(xf,str(pcdata),space,line_feed)
    endif
#endif

  end subroutine xml_AddPcdata_SP


  subroutine xml_AddPcdata_DP(xf,pcdata,fmt,space,line_feed)
    type(xmlf_t), intent(inout)    :: xf
    real(kind=dp), intent(in)      :: pcdata
    character(len=*), optional    :: fmt
    logical, intent(in), optional  :: space
    logical, intent(in), optional  :: line_feed

#ifdef PGF90
    if (present(fmt)) then
      call xml_AddPcdata_Ch(xf,trim(str(pcdata,fmt)),space,line_feed)
    else
      call xml_AddPcdata_Ch(xf,trim(str(pcdata)),space,line_feed)
    endif
#else
    if (present(fmt)) then
      call xml_AddPcdata_Ch(xf,str(pcdata,fmt),space,line_feed)
    else
      call xml_AddPcdata_Ch(xf,str(pcdata),space,line_feed)
    endif
#endif

  end subroutine xml_AddPcdata_DP


  subroutine xml_AddPcdata_log(xf,pcdata,fmt,space,line_feed)
    type(xmlf_t), intent(inout)    :: xf
    logical, intent(in)            :: pcdata
    character(len=*), intent(in), optional    :: fmt
    logical, intent(in), optional  :: space
    logical, intent(in), optional  :: line_feed

    if (present(fmt)) then
      call xml_AddPcdata_Ch(xf,str(pcdata,fmt),space,line_feed)
    else
      call xml_AddPcdata_Ch(xf,str(pcdata),space,line_feed)
    endif

  end subroutine xml_AddPcdata_log


  subroutine xml_AddPcdata_int(xf,pcdata,fmt,space,line_feed)
    type(xmlf_t), intent(inout)    :: xf
    integer, intent(in)            :: pcdata
    character(len=*), intent(in), optional    :: fmt
    logical, intent(in), optional  :: space
    logical, intent(in), optional  :: line_feed

    if (present(fmt)) then
      call xml_AddPcdata_Ch(xf,str(pcdata,fmt),space,line_feed)
    else
      call xml_AddPcdata_Ch(xf,str(pcdata),space,line_feed)
    endif

  end subroutine xml_AddPcdata_int


  !-------------------------------------------------------------------

  subroutine xml_AddAttribute_SP(xf,name,value,fmt)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name
    real(kind=sp),    intent(in)  :: value
    character(len=*), intent(in), optional   :: fmt

    if (present(fmt)) then
      call xml_AddAttribute_Ch(xf,name,str(value,fmt))
    else
      call xml_AddAttribute_Ch(xf,name,str(value))
    endif

  end subroutine xml_AddAttribute_SP

  subroutine xml_AddAttribute_DP(xf,name,value,fmt)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name
    real(kind=dp),    intent(in)  :: value
    character(len=*), intent(in), optional   :: fmt

    if (present(fmt)) then
      call xml_AddAttribute_Ch(xf,name,str(value,fmt))
    else
      call xml_AddAttribute_Ch(xf,name,str(value))
    endif

  end subroutine xml_AddAttribute_DP

  subroutine xml_AddAttribute_log(xf,name,value,fmt)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name
    logical, intent(in)           :: value
    character(len=*), intent(in), optional   :: fmt

    if (present(fmt)) then
      call xml_AddAttribute_Ch(xf,name,str(value,fmt))
    else
      call xml_AddAttribute_Ch(xf,name,str(value))
    endif

  end subroutine xml_AddAttribute_log

  subroutine xml_AddAttribute_int(xf,name,value,fmt)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name
    integer, intent(in)           :: value
    character(len=*), intent(in), optional   :: fmt

    if (present(fmt)) then
      call xml_AddAttribute_Ch(xf,name,str(value,fmt))
    else
      call xml_AddAttribute_Ch(xf,name,str(value))
    endif

  end subroutine xml_AddAttribute_int

  subroutine xml_AddPseudoAttribute_SP(xf, name, value)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name
    real(sp), intent(in)          :: value

    call xml_AddPseudoAttribute_Ch(xf, name, str(value))
  end subroutine xml_AddPseudoAttribute_SP

  subroutine xml_AddPseudoAttribute_DP(xf, name, value)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name
    real(dp), intent(in)          :: value

    call xml_AddPseudoAttribute_Ch(xf, name, str(value))
  end subroutine xml_AddPseudoAttribute_DP

  subroutine xml_AddPseudoAttribute_Int(xf, name, value)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name
    integer, intent(in)           :: value

    call xml_AddPseudoAttribute_Ch(xf, name, str(value))
  end subroutine xml_AddPseudoAttribute_Int

  subroutine xml_AddPseudoAttribute_Log(xf, name, value)
    type(xmlf_t), intent(inout)   :: xf
    character(len=*), intent(in)  :: name
    logical, intent(in)           :: value

    call xml_AddPseudoAttribute_Ch(xf, name, str(value))
  end subroutine xml_AddPseudoAttribute_Log

end module m_wxml_overloads

