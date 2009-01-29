module m_wkml_chart

  use m_common_error, only: FoX_error
  use FoX_wxml
  use m_wkml_lowlevel
  use FoX_common, only: str

  private

  public :: kmlAddChart

  interface kmlAddChart
   module procedure kmlAddChart_sp
   module procedure kmlAddChart_dp
  end interface kmlAddChart

contains

    subroutine kmlAddChart_sp(xf,charttype,chartsize,chartdata,chartscale,charttitle,chartlabel)
    
 
      type(xmlf_t), intent(inout) :: xf
      real,intent(in) :: chartdata(:)
      integer :: i
      character(len=*), intent(in) :: charttype,chartscale,chartsize,charttitle
      character(len=6),allocatable :: chartdata_chr(:)
!      character(len=*), intent(in),optional :: chartlabelx,chartlabely
      character(len=*), intent(in),optional :: chartlabel      

      print*,'run kmlAddChart_sp'    


      allocate(chartdata_chr(size(chartdata)))



      Do i=1, size(chartdata)
       chartdata_chr(i)=trim(str(chartdata(i),fmt="r4"))
      End do



     call xml_NewElement(xf,'description')
!      call xml_AddCharacters(xf,'<![CDATA[<img src="http://chart.apis.google.com/chart?"'//&
!"cht="//charttype//"&"//"chs="//chartsize//"&"//"chd=t:"//chartdata//"&"//"chds="//chartscale//&
!"chxt=x,y"//"chx1="//"0:"//"|Date|"//"1:"//"|Discharge|"//'"/>]]>')

     call xml_AddCharacters(xf,'<img src="http://chart.apis.google.com/chart?'//&
"cht="//trim(charttype)//"&"//"chs="//trim(chartsize)//"&"//&
"chd=t:"//trim(str(chartdata_chr,delimiter=","))&
//"&"//"chds="//trim(chartscale)//"&"&
//"chtt="//trim(charttitle)//"&"&
//"chxt=x,y"//"&"&
!//"chxl=0:|Jan|Feb|March|1:|0|1.0"&
//"chxl="//trim(chartlabel)//""&
//'"/>', parsed=.false.)

     call xml_EndElement(xf,'description')


    end subroutine kmlAddChart_sp

    subroutine kmlAddChart_dp(xf,charttype,chartsize,chartdata,chartscale,charttitle,chartlabel)

      type(xmlf_t), intent(inout) :: xf
      double precision,intent(in) :: chartdata(:)
      character(len=*), intent(in) :: charttype,chartscale,chartsize,charttitle
      character(len=6),allocatable :: chartdata_chr(:)
!      character(len=*), intent(in),optional :: chartlabelx,chartlabely
      character(len=*), intent(in),optional :: chartlabel

      print*,'run kmlAddChart_dp'  

      allocate(chartdata_chr(size(chartdata)))

      Do i=1, size(chartdata)
       chartdata_chr(i)=trim(str(chartdata(i),fmt="r4"))
      End do

     print*,'chartdata_chr',chartdata_chr


     call xml_NewElement(xf,'description')
!      call xml_AddCharacters(xf,'<![CDATA[<img src="http://chart.apis.google.com/chart?"'//&
!"cht="//charttype//"&"//"chs="//chartsize//"&"//"chd=t:"//chartdata//"&"//"chds="//chartscale//&
!"chxt=x,y"//"chx1="//"0:"//"|Date|"//"1:"//"|Discharge|"//'"/>]]>')

     call xml_AddCharacters(xf,'<img src="http://chart.apis.google.com/chart?'//&
"cht="//trim(charttype)//"&"//"chs="//trim(chartsize)//"&"//&
"chd=t:",parsed=.false.)
! this is to check whether we like the XML keep indent preserve_whitesapce=.true. means indent
! we do not want this here, just for data section. thus set wxml xf type and remove the private.
!

! in FoX 4.0.3 preserve_whitesapce=.true not exist anymore, probably need to use pretty_print = .false.

 
     print*,"PW", xf%pretty_print
     xf%pretty_print = .false.
     call xml_AddNewLine(xf)
     do i = 1, size(chartdata_chr)-1
       call xml_AddCharacters(xf,chartdata_chr(i)//",")
       call xml_AddNewLine(xf)
     enddo
     call xml_AddCharacters(xf,chartdata_chr(i))
     call xml_AddCharacters(xf,"&"//"chds="//trim(chartscale)//"&"&
//"chtt="//trim(charttitle)//"&"&
//"chxt=x,y"//"&"&
!//"chxl=0:|Jan|Feb|March|1:|0|1.0"&
//"chxl="//trim(chartlabel)//""&
//'"/>', parsed=.false.)
     xf%pretty_print = .false.

     call xml_EndElement(xf,'description')

    end subroutine kmlAddChart_dp


end module m_wkml_chart
