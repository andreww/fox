module m_wcml_coma
  ! Implements routines relating to the (currently unfinished)
  ! CML Condensed Matter schema

  use xmlf90_wxml, only: xmlf_t
  use xmlf90_wxml, only: xml_NewElement, xml_AddAttribute
  use xmlf90_wxml, only: xml_EndElement, xml_AddPcData
  use m_wcml_stml, only: stmAddValue
  use m_wxml_error
  
  Implicit None

  Private

  integer, private, parameter ::  sp = selected_real_kind(6,30)
  integer, private, parameter ::  dp = selected_real_kind(14,100)

  public :: cmlAddBand
  public :: cmlStartBandList
  public :: cmlEndBandList
  
  public :: cmlAddKPoint
  public :: cmlStartKPointList
  public :: cmlEndKPointList

  interface cmlAddBand
    module procedure cmlAddBand_kpt
    module procedure cmlAddBand_kptref
  end interface

contains
  
  subroutine cmlAddBand_kpt(xf, kpoint, kweight, bands, kptfmt, eigfmt)

    implicit none
    type(xmlf_t), intent(inout)            :: xf
    real(dp), intent(in) :: kpoint(3)
    real(dp), intent(in) :: kweight
    real(dp), intent(in) :: bands(:)
    character(len=*), intent(in), optional :: kptfmt
    character(len=*), intent(in), optional :: eigfmt
    
    integer :: i, n
    character(len=20) :: k_fmt
    character(len=20) :: kp_c(3)

    if (present(kptfmt)) then
      if (len_trim(kptfmt) > 20) stop 'k-point format too long' 
      k_fmt=kptfmt
    else
      k_fmt='(f10.7)'
    endif

    n = size(bands)

    do i = 1, 3
      write(kp_c(i), k_fmt, err=100) kpoint(i)
    enddo

    call xml_NewElement(xf, 'band')
    call xml_AddAttribute(xf, 'kpoint', trim(kp_c(1))//' '//trim(kp_c(2))//' '//trim(kp_c(3)))
    call xml_AddAttribute(xf, 'weight', kweight)
    call stmAddValue(xf, value=bands(1:n), fmt=eigfmt)
    call xml_EndElement(xf, 'band')

    return
100 call wxml_fatal('internal write failed in cmlAddBand')
  end subroutine cmlAddBand_kpt

  subroutine cmlAddBand_kptref(xf, kptref, bands, eigfmt)
 
    type(xmlf_t), intent(inout)            :: xf
    character(len=*), intent(in) :: kptref
    real(dp), intent(in) :: bands(:)
    character(len=*), intent(in), optional :: eigfmt
    
    integer :: n
    
    n = size(bands)
    
    call xml_NewElement(xf, 'band')
    call xml_AddAttribute(xf, 'kpointRef', kptref)
    call stmAddValue(xf, value=bands(1:n), fmt=eigfmt)
    call xml_EndElement(xf, 'band')
 
   end subroutine cmlAddBand_kptref
    

  subroutine cmlStartBandList(xf, id, title, conv, dictref, ref, role)

    type(xmlf_t), intent(inout) :: xf
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: ref
    character(len=*), intent(in), optional :: role
    
    call xml_NewElement(xf, 'bandList')
    if (present(id)) call xml_AddAttribute(xf, 'id', id)
    if (present(title)) call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
    if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
    if (present(role)) call xml_AddAttribute(xf, 'role', role)
    
  end subroutine cmlStartBandList


  subroutine cmlEndBandList(xf)

    implicit none
    type(xmlf_t), intent(inout) :: xf

    Call xml_EndElement(xf, 'bandList')
    
  end subroutine cmlEndBandList


  subroutine cmlAddKPoint(xf, kpoint, weight, id, title, conv, dictref, weightfmt, kptfmt)
    
    type(xmlf_t), intent(inout) :: xf
    real(kind=dp), dimension(3), intent(in)  :: kpoint
    real(kind=dp), intent(in)  :: weight
    character(len=*), intent(in), optional :: id
    character(len=*), intent(in), optional :: title
    character(len=*), intent(in), optional :: dictref
    character(len=*), intent(in), optional :: conv
    character(len=*), intent(in), optional :: weightfmt
    character(len=*), intent(in), optional :: kptfmt
    
    character(len=20) :: k_fmt
    character(len=20) :: weight_f
    
    if (present(kptfmt)) then
      if (len_trim(kptfmt) > 20) stop 'k-point format too long' 
      k_fmt=kptfmt
    else
      k_fmt="(f11.6)"
    endif
    
    if (present(weightfmt)) then
      if (len_trim(weightfmt) > 20) stop 'weight format too long' 
      write(weight_f,fmt=weightfmt,err=101) weight
    else
      write(weight_f,fmt='(f11.7)',err=101) weight
    endif
    
    call xml_NewElement(xf, 'kpoint')
    if (present(id))      call xml_AddAttribute(xf, 'id', id)
    if (present(title))   call xml_AddAttribute(xf, 'title', title)
    if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
    if (present(conv))    call xml_AddAttribute(xf, 'convention', conv)
    call xml_AddAttribute(xf, 'weight', trim(adjustl(weight_f)))
    call xml_AddPcdata(xf, kpoint(1))
    call xml_AddPcdata(xf, kpoint(2),space=.true.)
    call xml_AddPcdata(xf, kpoint(3),space=.true.)
    call xml_EndElement(xf, 'kpoint')
    
    return
101 call wxml_fatal('internal write failed in cmlAddKPoint')
   end subroutine cmlAddKPoint


   subroutine cmlStartKPointList(xf, id, title, conv, dictref, ref, role)
 
     type(xmlf_t), intent(inout) :: xf
     character(len=*), intent(in), optional :: id
     character(len=*), intent(in), optional :: title
     character(len=*), intent(in), optional :: conv
     character(len=*), intent(in), optional :: dictref
     character(len=*), intent(in), optional :: ref
     character(len=*), intent(in), optional :: role
     
     call xml_NewElement(xf, 'kpointList')
     if (present(id)) call xml_AddAttribute(xf, 'id', id)
     if (present(title)) call xml_AddAttribute(xf, 'title', title)
     if (present(dictref)) call xml_AddAttribute(xf, 'dictRef', dictref)
     if (present(conv)) call xml_AddAttribute(xf, 'convention', conv)
     if (present(ref)) call xml_AddAttribute(xf, 'ref', ref)
     if (present(role)) call xml_AddAttribute(xf, 'role', role)
     
   end subroutine cmlStartKPointList

 
   subroutine cmlEndKPointList(xf)
     
     implicit none
     type(xmlf_t), intent(inout) :: xf
     
     Call xml_EndElement(xf, 'kpointList')
     
   end subroutine cmlEndKPointList

end module m_wcml_coma
