module m_common_entity_expand

#ifndef DUMMYLIB
  use fox_m_fsys_vstr, only: new_vs, add_chars, vs, len, as_chars
  use m_common_entities, only: expand_char_entity
  use m_common_error, only: error_stack, add_error
  use m_common_namecheck, only: checkName, checkCharacterEntityReference, &
    checkRepCharEntityReference
  use m_common_struct, only: xml_doc_state

  implicit none
  private

  public :: expand_entity_value_alloc

  ! This does the first level of expansion of the contents of an entity
  ! reference, for storage during processing. Only character references
  ! are expanded.

contains

  function expand_entity_value_alloc(repl, xds, stack) result(repl_new)
    ! Perform expansion of character entity references
    ! check that no parameter entities are present
    ! and check that all general entity references are well-formed.
    ! before storing it.
    !
    ! These are usefull here - we don't do any allocation 
    ! just stuff things in an array.
    use fox_m_fsys_array_str, only: str_vs, vs_str

    type(vs), intent(in) :: repl
    type(xml_doc_state), intent(in) :: xds
    type(error_stack), intent(inout) :: stack
    type(vs), pointer :: repl_new

    character, dimension(len(repl)) :: repl_temp
    integer :: i, i2, j
    
    repl_new => new_vs()
    if (index(as_chars(repl),'%')/=0) then
      call add_error(stack, "Not allowed % in internal subset general entity value")
      return
    endif

    i = 1
    i2 = 1
    do
      if (i>len(repl)) exit
      if (as_chars(repl, i, i)=='&') then
        j = index(as_chars(repl, i+1, len(repl)),';')
        if (j==0) then
          call add_error(stack, "Not allowed bare & in entity value")
          return
        elseif (checkName(as_chars(repl,(i+1),(i+j-1)), xds%xml_version)) then
          repl_temp(i2:i2+j) = as_chars(repl,i,(i+j))
          i = i + j + 1
          i2 = i2 + j + 1
          ! For SAX, we need to be able to represent the character:
        elseif (checkRepCharEntityReference(as_chars(repl,i+1,i+j-1), xds%xml_version)) then
          !if it is ascii then
          repl_temp(i2:i2) = vs_str(expand_char_entity(as_chars(repl,i+1,i+j-1)))
          i = i + j + 1
          i2 = i2 + 1
        elseif (checkCharacterEntityReference(as_chars(repl,i+1,i+j-1), xds%xml_version)) then
          ! We can't represent it. Issue an error and stop.
          call add_error(stack, "Unable to digest character entity reference in entity value, sorry.")
          return
        else
          call add_error(stack, "Invalid entity reference in entity value")
          return
        endif
      else
        repl_temp(i2) = as_chars(repl,i,i)
        i = i + 1
        i2 = i2 + 1
      endif
    enddo

    call add_chars(repl_new, str_vs(repl_temp(:i2-1)))

  end function expand_entity_value_alloc

#endif
end module m_common_entity_expand
