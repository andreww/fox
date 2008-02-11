program dom_example

  use FoX_dom
  implicit none

  type(Node), pointer :: myDoc
  type(DOMConfiguration), pointer :: dc
  real :: t1, t2

  call cpu_time(t1)
  ! Load in the document
  dc => newDOMConfig()
  call setParameter(dc, "namespaces", .true.)

  myDoc => parseFile("test.xml", dc)

  ! Tell the normalizer to canonicalize it
  call setParameter(getDomConfig(myDoc), "canonical-form", .true.)

  ! and write it out again (which automatically does normalization)
  call serialize(myDoc, "out.xml")
  call cpu_time(t2)

  print*, "Finished"
  print*, "Elapsed time:", t2-t1

  ! Clear up all allocated memory
  call destroy(myDoc)
  call destroy(dc)
end program dom_example
