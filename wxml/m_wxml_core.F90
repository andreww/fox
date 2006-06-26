module m_wxml_core

use m_common_array_str, only: assign_array_to_str
use m_common_array_str, only: assign_str_to_array
use m_common_elstack
use m_common_error, only: FoX_warning_base, FoX_error_base, FoX_fatal_base
use m_common_buffer
use m_common_attrs
use m_wxml_escape, only: check_Name, escape_string, escape_string_len
use m_wxml_text, only: str

use pxf, only: pxfabort

implicit none

logical, parameter  :: pcdata_advance_line_default = .false.
logical, parameter  :: pcdata_advance_space_default = .false.

integer, parameter ::  sp = selected_real_kind(6,30)
integer, parameter ::  dp = selected_real_kind(14,100)

private

character(len=*), public, parameter :: FoX_version = '2.0'

!Output State Machines
! status wrt root element:
integer, parameter :: WXML_STATE_1_JUST_OPENED = 0 
  !File is just opened, nothing written to it yet.
integer, parameter :: WXML_STATE_1_BEFORE_ROOT = 1
  !File has been opened, something has been written, but no root element yet.
integer, parameter :: WXML_STATE_1_DURING_ROOT = 2
  !The root element has been opened but not closed
integer, parameter :: WXML_STATE_1_AFTER_ROOT = 3
  !The root element has been opened but not closed

! status wrt tags:
integer, parameter :: WXML_STATE_2_OUTSIDE_TAG= 0
  !We are not within a tag.
integer, parameter :: WXML_STATE_2_INSIDE_PI = 1
  !We are inside a Processing Instruction tag
integer, parameter :: WXML_STATE_2_INSIDE_ELEMENT = 2
  !We are in inside an element tag.


type, public :: xmlf_t
   character, pointer      :: filename(:)
   integer                 :: lun
   type(buffer_t)          :: buffer
   type(elstack_t)         :: stack
   type(dictionary_t)      :: dict
   integer                 :: state_1
   integer                 :: state_2
   logical                 :: indenting_requested
end type xmlf_t

public :: xml_OpenFile, xml_NewElement, xml_EndElement, xml_Close
public :: xml_AddXMLDeclaration
public :: xml_AddXMLStylesheet
public :: xml_AddXMLPI
public :: xml_AddComment
public :: xml_AddCdataSection
public :: xml_AddPcdata
public :: xml_AddAttribute
public :: xml_AddPseudoAttribute
 
interface xml_AddPcdata
  module procedure xml_AddPcdata_Ch
end interface
interface xml_AddAttribute
  module procedure xml_AddAttribute_Ch
end interface
interface xml_AddPseudoAttribute
  module procedure xml_AddPseudoAttribute_Ch
end interface
 
!public :: xml_AddArray

!overload error handlers to allow file info
interface wxml_warning
  module procedure wxml_warning_xf, FoX_warning_base
end interface
interface wxml_error
  module procedure wxml_error_xf, FoX_error_base
end interface
interface wxml_fatal
  module procedure wxml_fatal_xf, FoX_fatal_base
end interface

!
! Heuristic (approximate) target for justification of output
! Large unbroken pcdatas will go beyond this limit
! 
integer, parameter  :: COLUMNS = 80

! TOHW - This is the longest string that may be output without
! a newline. The buffer must not be larger than this, but its size 
! can be tuned for performance.
integer, parameter  :: xml_recl = 4096

CONTAINS

!-------------------------------------------------------------------
subroutine xml_OpenFile(filename, xf, indent, channel, replace, addDecl)
character(len=*), intent(in)  :: filename
type(xmlf_t), intent(inout)   :: xf
logical, intent(in), optional :: indent
integer, intent(in), optional :: channel
logical, intent(in), optional :: replace
logical, intent(in), optional :: addDecl

integer :: iostat
logical :: repl, decl

if (present(replace)) then
       repl = replace
else
       repl = .true.
endif
if (present(addDecl)) then
       decl = addDecl
else
       decl = .true.
endif

allocate(xf%filename(len(filename)))
call assign_str_to_array(xf%filename,filename)

if (present(channel)) then
  xf%lun = channel
else
  call get_unit(xf%lun,iostat)
  if (iostat /= 0) call wxml_fatal(xf, "cannot open file")
endif

!
! Use large I/O buffer in case the O.S./Compiler combination
! has hard-limits by default (i.e., NAGWare f95's 1024 byte limit)
! This is related to the maximum size of the buffer.
! TOHW - This is the longest string that may be output without
! a newline. The buffer must not be larger than this, but its size 
! can be tuned for performance.

if(repl) then
  open(unit=xf%lun, file=filename, form="formatted", status="unknown", &
       action="write", position="rewind", recl=xml_recl)
else 
  open(unit=xf%lun, file=filename, form="formatted", status="old", &
       action="write", position="append", recl=xml_recl)
endif

call init_elstack(xf%stack)

call init_dict(xf%dict)
call reset_buffer(xf%buffer)

xf%state_1 = WXML_STATE_1_JUST_OPENED
xf%state_2 = WXML_STATE_2_OUTSIDE_TAG

xf%indenting_requested = .false.
if (present(indent)) then
   xf%indenting_requested = indent
endif

if (decl) then
  call xml_AddXMLDeclaration(xf,encoding='UTF-8')
endif

end subroutine xml_OpenFile


subroutine xml_AddXMLDeclaration(xf, encoding, standalone)
  type(xmlf_t), intent(inout)   :: xf
  character(len=*), intent(in), optional :: encoding
  logical, intent(in), optional :: standalone

  if (xf%state_1 /= WXML_STATE_1_JUST_OPENED) &
    call wxml_error("Tried to put XML declaration in wrong place")

  call xml_AddXMLPI(xf, "xml")
  call xml_AddPseudoAttribute(xf, "version", "1.0")
  if (present(encoding)) &
    call xml_AddPseudoAttribute(xf, "encoding", encoding)
  if (present(standalone)) then
    if (standalone) then
      call xml_AddPseudoAttribute(xf, "standalone", "yes")
    else
      call xml_AddPseudoAttribute(xf, "standalone", "no")
    endif
  endif
  xf%state_1 = WXML_STATE_1_BEFORE_ROOT
end subroutine xml_AddXMLDeclaration


subroutine xml_AddXMLStylesheet(xf, href, type, title, media, charset, alternate)
  type(xmlf_t), intent(inout)   :: xf
  character(len=*), intent(in) :: href
  character(len=*), intent(in) :: type
  character(len=*), intent(in), optional :: title
  character(len=*), intent(in), optional :: media
  character(len=*), intent(in), optional :: charset
  logical,          intent(in), optional :: alternate

  call close_start_tag(xf)

  call xml_AddXMLPI(xf, 'xml-stylesheet')
  call xml_AddPseudoAttribute(xf, 'href', href)
  call xml_AddPseudoAttribute(xf, 'type', type)

  if (present(title)) call xml_AddPseudoAttribute(xf, 'title', title)
  if (present(media)) call xml_AddPseudoAttribute(xf, 'media', media)
  if (present(charset)) call xml_AddPseudoAttribute(xf, 'charset', charset)
  if (present(alternate)) then
    if (alternate) then
      call xml_AddPseudoAttribute(xf, 'alternate', 'yes')
    else
      call xml_AddPseudoAttribute(xf, 'alternate', 'no')
    endif
  endif
  if (xf%state_1 == WXML_STATE_1_JUST_OPENED) &
    xf%state_1 = WXML_STATE_1_BEFORE_ROOT 
  xf%state_2 = WXML_STATE_2_INSIDE_PI

end subroutine xml_AddXMLStylesheet


subroutine xml_AddXMLPI(xf, name, data)
  type(xmlf_t), intent(inout)            :: xf
  character(len=*), intent(in)           :: name
  character(len=*), intent(in), optional :: data

  call close_start_tag(xf)
  if (xf%state_1 == WXML_STATE_1_JUST_OPENED) then
    xf%state_1 = WXML_STATE_1_BEFORE_ROOT
  else
    call add_eol(xf)
  endif

  call add_to_buffer("<?" // name, xf%buffer)
  if (present(data)) then
    if (index(data, '?>') > 0) &
      call wxml_error("Tried to output invalid PI data")
    call add_to_buffer(' '//data//'?>', xf%buffer)
! state_2 is now OUTSIDE_TAG from close_start_tag
  else
    xf%state_2 = WXML_STATE_2_INSIDE_PI
    call reset_dict(xf%dict)
  endif

end subroutine xml_AddXMLPI


subroutine xml_AddComment(xf,comment)
  type(xmlf_t), intent(inout)   :: xf
  character(len=*), intent(in)  :: comment

  if (index(comment,'--') > 0 .or. comment(len(comment):) == '-') &
    call wxml_error("Tried to output invalid comment")

  call close_start_tag(xf)
  call add_eol(xf)
  call add_to_buffer("<!--", xf%buffer)
  call add_to_buffer(comment, xf%buffer)
  call add_to_buffer("-->", xf%buffer)
  if (xf%state_1 == WXML_STATE_1_JUST_OPENED) &
    xf%state_1 = WXML_STATE_1_BEFORE_ROOT

end subroutine xml_AddComment

!NB No necessity for a processor to output CDATA on request;
! it is at the discretion of the processor whether character 
! data is CDATA or PCDATA. Therefore don't bother.
!-------------------------------------------------------------------
subroutine xml_AddCdataSection(xf,cdata)
type(xmlf_t), intent(inout)   :: xf
character(len=*), intent(in)  :: cdata
!
if (index(cdata,']]>') > 0) &
   call wxml_error("Tried to output invalid CDATA")
!
!TOHW FIXMW state machine
!
!call close_start_tag(xf)
!call add_to_buffer("<![CDATA[", xf%buffer)
!call add_to_buffer(cdata, xf%buffer)
!@call add_to_buffer("]]>", xf%buffer)
end subroutine xml_AddCdataSection

!-------------------------------------------------------------------
subroutine xml_NewElement(xf,name)
type(xmlf_t), intent(inout)   :: xf
character(len=*), intent(in)  :: name

  select case (xf%state_1)
  case (WXML_STATE_1_JUST_OPENED)
    xf%state_1 = WXML_STATE_1_DURING_ROOT
  case (WXML_STATE_1_BEFORE_ROOT)
    xf%state_1 = WXML_STATE_1_DURING_ROOT
  case (WXML_STATE_1_DURING_ROOT)
    continue
  case (WXML_STATE_1_AFTER_ROOT)
    call wxml_error(xf, "two root elements")
  end select 

  if (.not.check_Name(name)) then
    call wxml_warning(xf, 'attribute name '//name//' is not valid')
  endif

  call close_start_tag(xf)
  call add_eol(xf)
  call push_elstack(name,xf%stack)
  call add_to_buffer("<"//name, xf%buffer)
  xf%state_2 = WXML_STATE_2_INSIDE_ELEMENT
  call reset_dict(xf%dict)

end subroutine xml_NewElement
!-------------------------------------------------------------------
subroutine xml_AddPcdata_Ch(xf,pcdata,space,line_feed)
  type(xmlf_t), intent(inout)   :: xf
  character(len=*), intent(in)  :: pcdata
  logical, intent(in), optional  :: space
  logical, intent(in), optional  :: line_feed

  logical :: advance_line , advance_space

!TOHW FIXME we should never do this inside an
! element but I think we might.
! also we might want to output CDATA if there's too
! much quoting going on??
! Rename to CharData?

  if (xf%state_1 /= WXML_STATE_1_DURING_ROOT) &
    call wxml_error("Cannot output character data here.")

  advance_line = pcdata_advance_line_default 
  if (present(line_feed)) then
    advance_line = line_feed
  endif

  advance_space = pcdata_advance_space_default 
  if (present(space)) then
    advance_space = space
  endif

  call close_start_tag(xf)

  if (advance_line) then
    call add_eol(xf)
    advance_space = .false.
  else
    if (xf%indenting_requested) then
      if ((len(xf%buffer) + escape_string_len(pcdata) + 1) > COLUMNS ) then
        call add_eol(xf)
        advance_space = .false.
      endif
    endif
  endif
  if (advance_space) call add_to_buffer(" ",xf%buffer)

  call add_to_buffer(escape_String(pcdata), xf%buffer)

end subroutine xml_AddPcdata_Ch

subroutine xml_AddAttribute_Ch(xf,name,value)
  type(xmlf_t), intent(inout)   :: xf
  character(len=*), intent(in)  :: name
  character(len=*), intent(in)  :: value

  if (xf%state_2 /= WXML_STATE_2_INSIDE_ELEMENT) &
    call wxml_error(xf, "attributes outside element content")

  if (has_key(xf%dict,name)) then
    call wxml_error(xf, "duplicate att name")
  endif

  call add_item_to_dict(xf%dict, name, value)

end subroutine xml_AddAttribute_Ch

subroutine xml_AddPseudoAttribute_Ch(xf, name, value)
  type(xmlf_t), intent(inout)   :: xf
  character(len=*), intent(in)  :: name
  character(len=*), intent(in)  :: value

  if (xf%state_2 /= WXML_STATE_2_INSIDE_PI) &
     call wxml_fatal("PI pseudo-attribute outside PI")

  if (has_key(xf%dict,name)) &
     call wxml_error(xf, "duplicate PI pseudo-attribute name")
  
  call add_item_to_dict(xf%dict, name, value)

end subroutine xml_AddPseudoAttribute_Ch

subroutine xml_AddArray_Ch(xf, array)
  type(xmlf_t), intent(inout) :: xf
  character(len=*), dimension(:), intent(in) :: array

  integer :: i,n
  !If appropriate

  n = size(array)
  do i = 1, n-1
    call add_to_buffer(array(i)//' ', xf%buffer)
  enddo
  call add_to_buffer(array(n), xf%buffer)

end subroutine xml_AddArray_Ch  

!-----------------------------------------------------------
subroutine xml_EndElement(xf,name)
  type(xmlf_t), intent(inout)   :: xf
  character(len=*), intent(in)  :: name

  if (get_top_elstack(xf%stack) /= name) &
    call wxml_fatal(xf, 'Trying to close '//name//' but '//get_top_elstack(xf%stack)//' is open.') 
  call devnull(pop_elstack(xf%stack))
  if (is_empty(xf%stack)) then
    xf%state_1 = WXML_STATE_1_AFTER_ROOT
  endif

  select case (xf%state_2)
  case (WXML_STATE_2_INSIDE_ELEMENT)
    if (len(xf%dict) > 0) call write_attributes(xf)
    call add_to_buffer("/>",xf%buffer)
  case (WXML_STATE_2_OUTSIDE_TAG)
    call add_eol(xf)
    call add_to_buffer("</" // name // ">", xf%buffer)
  case default
    call wxml_error("Cannot close element here")
  end select
  xf%state_2 = WXML_STATE_2_OUTSIDE_TAG

end subroutine xml_EndElement

!----------------------------------------------------------------

subroutine xml_Close(xf)
type(xmlf_t), intent(inout)   :: xf

  call close_start_tag(xf)

  do while (xf%state_1 == WXML_STATE_1_DURING_ROOT)
    if (xf%state_1 == WXML_STATE_1_AFTER_ROOT) exit
    call xml_EndElement(xf, get_top_elstack(xf%stack))
  enddo

  !should call dump_buffer
  write(unit=xf%lun,fmt="(a)") char(xf%buffer)
  close(unit=xf%lun)

  call destroy_dict(xf%dict)
  call destroy_elstack(xf%stack)

  deallocate(xf%filename)

end subroutine xml_Close

!==================================================================
!-------------------------------------------------------------------
subroutine get_unit(lun,iostat)

! Get an available Fortran unit number

integer, intent(out)  :: lun
integer, intent(out)  :: iostat

integer :: i
logical :: unit_used

do i = 10, 99
   lun = i
   inquire(unit=lun,opened=unit_used)
   if (.not. unit_used) then
      iostat = 0
      return
   endif
enddo
iostat = -1
lun = -1
end subroutine get_unit

!----------------------------------------------------------
subroutine add_eol(xf)
type(xmlf_t), intent(inout)   :: xf

integer :: indent_level

! In case we still have a zero-length stack, we must make
! sure indent_level is not less than zero.
indent_level = max(len(xf%stack) - 1, 0)

!We must flush here (rather than just adding an eol character)
!since we don't know what the eol character is on this system.
!Flushing with a linefeed will get it automatically, though.
write(unit=xf%lun,fmt="(a)") char(xf%buffer)
call reset_buffer(xf%buffer)

if (xf%indenting_requested) &
   call add_to_buffer(repeat(' ',indent_level),xf%buffer)

end subroutine add_eol
!------------------------------------------------------------
subroutine dump_buffer(xf,lf)
type(xmlf_t), intent(inout)   :: xf
logical, intent(in), optional :: lf

if (present(lf)) then
   if (lf) then
      write(unit=xf%lun,fmt="(a)",advance="yes") char(xf%buffer)
   else
      write(unit=xf%lun,fmt="(a)",advance="no") char(xf%buffer)
   endif
else
   write(unit=xf%lun,fmt="(a)",advance="no") char(xf%buffer)
endif
call reset_buffer(xf%buffer)

end subroutine dump_buffer

!------------------------------------------------------------
subroutine close_start_tag(xf)
type(xmlf_t), intent(inout)   :: xf

  select case (xf%state_2)
  case (WXML_STATE_2_INSIDE_ELEMENT)
    if (len(xf%dict) > 0)  call write_attributes(xf)
    call add_to_buffer('>', xf%buffer)
  case (WXML_STATE_2_INSIDE_PI)
    if (len(xf%dict) > 0)  call write_attributes(xf)
    call add_to_buffer('?>', xf%buffer)
  case (WXML_STATE_2_OUTSIDE_TAG)
    continue
  case default
    call wxml_fatal("Internal library error")
  end select
  
  xf%state_2 = WXML_STATE_2_OUTSIDE_TAG
  
end subroutine close_start_tag

!-------------------------------------------------------------
subroutine write_attributes(xf)
type(xmlf_t), intent(inout)   :: xf

integer  :: i, size

if (xf%state_2 /= WXML_STATE_2_INSIDE_PI .and. &
    xf%state_2 /= WXML_STATE_2_INSIDE_ELEMENT) &
  call wxml_fatal("Internal library error")

do i = 1, len(xf%dict)
   size = len(get_key(xf%dict, i)) + escape_string_len(get_value(xf%dict, i)) + 4
   if ((len(xf%buffer) + size) > COLUMNS) call add_eol(xf)
   call add_to_buffer(" ", xf%buffer)
   call add_to_buffer(get_key(xf%dict, i), xf%buffer)
   call add_to_buffer("=", xf%buffer)
   call add_to_buffer("""",xf%buffer)
   call add_to_buffer(escape_string(get_value(xf%dict, i)), xf%buffer)
   call add_to_buffer("""", xf%buffer)
enddo

end subroutine write_attributes

!---------------------------------------------------------
! Error handling/trapping routines:

    subroutine wxml_warning_xf(xf, msg)
      ! Emit warning, but carry on.
      type(xmlf_t), intent(in) :: xf
      character(len=*), intent(in) :: msg

      write(6,'(a)') 'WARNING(wxml) in writing to file ', xmlf_name(xf)
      write(6,'(a)')  msg

    end subroutine wxml_warning_xf

    subroutine wxml_error_xf(xf, msg)
      ! Emit error message, clean up file and stop.
      type(xmlf_t), intent(inout) :: xf
      character(len=*), intent(in) :: msg

      write(6,'(a)') 'ERROR(wxml) in writing to file ', xmlf_name(xf)
      write(6,'(a)')  msg

      call xml_Close(xf)
      stop

    end subroutine wxml_error_xf

    subroutine wxml_fatal_xf(xf, msg)
      !Emit error message and abort with coredump. Does not try to
      !close file, so should be used from anything xml_Close might
      !itself call (to avoid infinite recursion!)

      type(xmlf_t), intent(in) :: xf
      character(len=*), intent(in) :: msg

      write(6,'(a)') 'ERROR(wxml) in writing to file ', xmlf_name(xf)
      write(6,'(a)')  msg

      call pxfabort()
      stop

    end subroutine wxml_fatal_xf

    pure function xmlf_name(xf) result(fn)
      Type (xmlf_t), intent(in) :: xf
      character(len=size(xf%filename)) :: fn
      call assign_array_to_str(fn,xf%filename)
    end function xmlf_name
      

    subroutine devnull(str)
      character(len=*), intent(in) :: str
      continue
    end subroutine devnull

end module m_wxml_core

