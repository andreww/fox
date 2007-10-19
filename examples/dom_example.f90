program dom_example

  use FoX_dom
  implicit none

  type(Node), pointer :: myDoc
  real :: t1, t2

  call cpu_time(t1)
  ! Load in the document
  myDoc => parseFile("h2o.xml")

  ! Tell the normalizer to canonicalize it
  call setParameter(getDomConfig(myDoc), "canonical-form", .true.)

  ! and write it out again (which automatically does normalization.
  call serialize(myDoc, "out.xml")
  call cpu_time(t2)

  print*, "Elapsed time:", t2-t1

  ! Clear up all allocated memory
  call destroy(myDoc)
end program dom_example
