program dom_example

  use FoX_dom
  implicit none

  type(Node), pointer :: myDoc, p
  type(NodeList), pointer :: parameterList
  integer :: i
  character :: c

  ! Load in the document
  myDoc => parseFile("h2o.xml")

  ! Find all the parameters:
  parameterList => getElementsByTagNameNS(myDoc, &
    "http://www.xml-cml.org/schema", "parameter")

  print*, "Found ", getLength(parameterList), " parameters."

  ! Loop over the parameter list. Note that the DOM
  ! counts from zero, not from one.
  do i = 0, getLength(parameterList)-1
    p => item(parameterList, i)
    ! Check for the existence of the attribute we're looking for
    if (hasAttribute(p, "name")) then
      ! and print out its value
      print*, "Parameter ", i, " is named ", getAttribute(p, "name")
    endif
  enddo

  ! Clear up all allocated memory
  call destroy(myDoc)
end program dom_example
