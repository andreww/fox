module m_wcml_inputdec

    use FoX_wxml, only: xmlf_t, xml_NewElement, xml_AddAttribute, &
                        xml_AddCharacters, xml_EndElement
    use m_common_error, only: FoX_error
    use m_common_io, only: get_unit
    use m_wcml_metadata
    use m_wcml_lists

    implicit none

    integer, parameter :: WCML_DUMP_NOFILE = -10
    integer, parameter :: WCML_DUMP_ISOPEN = -20
    integer, parameter :: WCML_DUMP_NOUNIT = -30

    interface wcmlDumpDec
        module procedure wcmlDumpDec_single
        module procedure wcmlDumpDec_array
    end interface wcmlDumpDec

    contains 

    subroutine wcmlDumpDec_single(xf, inputDec, line_lengths, trim_lines, dicRef, &
                           iostat)

        type(xmlf_t), intent(inout)    :: xf
        character(len=*), intent(in)   :: inputDec
        integer, intent(in)            :: line_lengths
        logical, intent(in)            :: trim_lines
        character(len=*), intent(in)   :: dicRef
        integer, intent(out), optional :: iostat

        call wcmlStartDecList(xf)
        call wcmlDumpDec_core(xf, inputDec, line_lengths, trim_lines, dicRef, iostat)
        call wcmlEndDecList(xf)

    end subroutine wcmlDumpDec_single

    subroutine wcmlDumpDec_array(xf, inputDec, line_lengths, trim_lines, dicRef, &
                           iostat)

        type(xmlf_t), intent(inout)    :: xf
        character(len=*), intent(in)   :: inputDec(:)
        integer, intent(in)            :: line_lengths(:)
        logical, intent(in)            :: trim_lines(:)
        character(len=*), intent(in)   :: dicRef(:)
        integer, intent(out), optional :: iostat

        integer :: i

        call wcmlStartDecList(xf)
        do i=1, size(inputDec)
            ! Need to check all arrays are the same lenght
            call wcmlDumpDec_core(xf, inputDec(i), line_lengths(i), trim_lines(i), dicRef(i), iostat)
            if (present(iostat)) then
                if (iostat /= 0) return
            endif
        enddo
        call wcmlEndDecList(xf)

    end subroutine wcmlDumpDec_array
                              

    subroutine wcmlDumpDec_core(xf, inputDec, line_lengths, trim_lines, dicRef, &
                           iostat)

        type(xmlf_t), intent(inout)    :: xf
        character(len=*), intent(in)   :: inputDec
        integer, intent(in)            :: line_lengths
        logical, intent(in)            :: trim_lines
        character(len=*), intent(in)   :: dicRef
        integer, intent(out), optional :: iostat

        character(len=line_lengths)  :: this_line
        integer                      :: ios

        logical                      :: ex
        logical                      :: op

        integer                      :: iostat_
        integer                      :: unit_
       
        if (present(iostat)) iostat = 0 ! will set for errors
 
        ! Check the file is not open and that it exists. 
        inquire( file=inputDec, iostat=iostat_, exist=ex, opened=op) 

        if (iostat_ /= 0) then
            ! inquire failed: probably system issue
            ! op and ex are not set. Bung out.
            if (present(iostat)) then
                iostat = iostat_
                return
            else
                call FoX_error("Inquire failed in wcmlDumpDec")
            endif
        endif

        if (.not.ex) then
            ! If the file name argument does not exist we cannot
            ! dump it. Bung out.
            if (present(iostat)) then
                iostat = WCML_DUMP_NOFILE
                return
            else 
                call FoX_error("File does not exist in wcmlDumpDec")
            endif
        endif

        if (op) then
            ! If the file is open (is attached to a unit number) we
            ! cannot do very much with it. We could (possibly) rewind
            ! and dump the contents but the caller would then have the
            ! file in a different place to where it started (and could
            ! miss input). We cannot work out where we are in the file
            ! the put things back to how we found them because 
            ! inquire does not tell us what we need. We cannot open the
            ! file on a second unit number as this is not permited in
            ! Fortran 95. So bung out.
            if (present(iostat)) then
                iostat = WCML_DUMP_ISOPEN
                return
            else 
                call FoX_error("File already open wcmlDumpDec")
            endif
        endif

        ! need to select a unit number - use common's get_unit for this
        call get_unit(unit_,iostat_)
        if (iostat_ /= 0) then
            ! Common only ever returns 0 or -1; not helpful for our caller
            if (present(iostat)) then
                iostat = WCML_DUMP_NOUNIT
                return
            else
                call FoX_error("Could not get_unt in wcmlDumpDec")
            endif
        endif

        ! OK, we can not open the file - checking that it worked
        open(unit=unit_, iostat=iostat_, file=inputDec, action='read')
        if (iostat_ /= 0) then
            ! Open failed. File recently deleted, for example.
            if (present(iostat)) then
                iostat = WCML_DUMP_NOUNIT
                return
            else
                call FoX_error("Could not open file in wcmlDumpDec")
            endif
        endif
        
        ! Now ready to go.    
        ! Start of CML output for this file
        call wcmlStartDec(xf, inputDec, dicRef)

        ! Foeach line in file
        ! dump line in <scalar>
        do
            read(unit_, '(a)' , iostat=ios) this_line
            if(ios.lt.0) then
                exit ! End of file
            elseif(ios.gt.0) then
                ! Error condition. 
                if (present(iostat)) then
                    iostat = ios
                    exit ! We will close tags and return
                         ! with iostat set.
                else
                    ! Just error out
                    call FoX_error("Error reading file in wcmlDumpDec")
                endif
            else
                call wcmlAddDecLine(xf, this_line)
            endif
        enddo

        ! End of CML output for this file
        call wcmlEndDec(xf)

        close(unit_, iostat=ios)
        !ios just to supress errors we cannot do anythin 
        !about. Only expect to see these if we are 
        !returning iostat != 0 anyway.

    end subroutine wcmlDumpDec_core

    subroutine wcmlStartDecList(xf, dictRef)
        type(xmlf_t), intent(inout)  :: xf
        character(len=*), intent(in), optional :: dictRef

        call cmlStartModule(xf, title="Input Data Files", role="LexicalFileList", dictRef=dictRef)

    end subroutine wcmlStartDecList

    subroutine wcmlEndDecList(xf)
        type(xmlf_t), intent(inout)  :: xf

        call cmlEndModule(xf)

    end subroutine wcmlEndDecList


    subroutine wcmlStartDec(xf, filename, dicRef)

        type(xmlf_t), intent(inout)  :: xf
        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dicRef

        ! FIXME - what to do with the dicRef?
        call cmlStartModule(xf, title="Input Dec", role="Lexical file")
            call cmlStartMetadataList(xf)
                call cmlAddMetadata(xf, 'filename', filename) !FIXME - DC term?
                                                              !FIXME - other info?
            call cmlEndMetadataList(xf)

    end subroutine wcmlStartDec

    subroutine wcmlAddDecLine(xf, text)
        type(xmlf_t), intent(inout)  :: xf
        character(len=*), intent(in) :: text

        call xml_NewElement(xf, name='scalar')
            call xml_AddAttribute(xf, name='dataType', value='xsd:string')
            call xml_AddCharacters(xf, chars=text, ws_significant=.true.)
        call xml_EndElement(xf, name='scalar')

    end subroutine wcmlAddDecLine

    subroutine wcmlEndDec(xf)
        type(xmlf_t), intent(inout)  :: xf
        call cmlEndModule(xf)
    end subroutine wcmlEndDec

end module m_wcml_inputdec
