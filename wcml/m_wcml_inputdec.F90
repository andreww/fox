module m_wcml_inputdec

    use FoX_wxml, only: xmlf_t, xml_NewElement, xml_AddAttribute, &
                        xml_AddCharacters, xml_EndElement
    use m_wcml_metadata
    use m_wcml_lists

    implicit none

    contains 

    subroutine wcmlDumpDec(xf, inputDec, line_lengths, trim_lines, dicRef)

        type(xmlf_t), intent(inout)  :: xf
        character(len=*), intent(in) :: inputDec
        integer, intent(in)          :: line_lengths
        logical, intent(in)          :: trim_lines
        character(len=*), intent(in) :: dicRef

        character(len=line_lengths)  :: this_line

        integer                      :: ios
        logical                      :: ex
        logical                      :: op
        integer                      :: un
        integer                      :: nr
        

        ! Find out if file called inputDec is open
        ! exists etc... select...

        inquire( file=inputDec, iostat=ios, exist=ex, opened=op, number=un, &
                 nextrec=nr ) ! And I'll need more

        ! If needed record position, if not open file
        if (.not.ex) &
            return ! File does not exisit - error needed.

        if (op) then
            ! need to store data etc. but for now
            return
        else
            ! need to select a unit number!
            open(unit=998, iostat=ios, file=inputDec, action='read')
        endif
            
        ! Start of CML output for this file
        call wcmlStartDec(xf, inputDec, dicRef)

        ! Foeach line in file
        ! dump line in <scalar>
        do
            read(998, '(a)' , iostat=ios) this_line
            if(ios.lt.0) then
                exit ! End of file
            elseif(ios.gt.0) then
                !Error condition
                print*, 'ERROR'
                exit ! Need to clean up and error out?
            else
                call wcmlAddDecLine(xf, this_line)
            endif
        enddo

        ! End of CML output for this file
        call wcmlEndDec(xf)

        ! Close, rewind, clean up 
        ! FIXME - sort out what to do if op is true.
        close(998)

    end subroutine wcmlDumpDec

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
