program dom_example

  use FoX_dom
  implicit none

  type(Node), pointer :: myDoc
  type(DOMConfiguration), pointer :: dc
  integer :: ios
  real :: t1, t2

  call cpu_time(t1)
  ! Load in the document
  dc => newDOMConfig()
  call setParameter(dc, "namespaces", .true.)

  myDoc => parseFile("test.xml", dc, iostat=ios)

  if (ios==0) then
    ! Tell the normalizer to canonicalize it
    ! but only if we are 1.0. 1.1 cannot be canonicalized.
    if (getXMLVersion(myDoc)=="1.0") &
      call setParameter(getDomConfig(myDoc), "canonical-form", .true.)

    ! and write it out again (which automatically does normalization)
    call serialize(myDoc, "out.xml")
  endif
  call cpu_time(t2)

  print*, "Finished"
  print*, "Elapsed time:", t2-t1

  ! Clear up all allocated memory
  call destroy(myDoc)
  call destroy(dc)
end program dom_example
