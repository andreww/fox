module m_dom_processing_instruction

  use m_common_array_str, only: str_vs

  use m_dom_types, only: Node, PROCESSING_INSTRUCTION_NODE

  implicit none
  private

  public :: getTarget

contains

  function getTarget(arg) result(c)
    type(Node), intent(in) :: arg
    character(len=size(arg%nodeName)) :: c

    if (arg%nodeType/=PROCESSING_INSTRUCTION_NODE) then
      ! FIXME error
    endif

    c = str_vs(arg%nodeName)
  end function getTarget

end module m_dom_processing_instruction
