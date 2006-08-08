 program wcml_example

   use FoX_wcml

  integer,  parameter ::  sp = selected_real_kind(6,30)
  integer,  parameter ::  dp = selected_real_kind(14,100)
!
! NB normally you will be writting to the xml file
! from mulitple fortran files/subroutines, therefore
! type(xmlf_t) :: myfile   (below)
! would normally need to be treated as a global
! variable, either in a module or a common block.
! 
  type(xmlf_t) :: myfile
!

  integer           :: num, na
  character(len=2)  :: elements(3)
  real(kind=dp)      :: coords(3,3)
  real(kind=dp)      :: adp

  data coords(1:3,1)/0.0d0, 0.0d0, 0.0d0/
  data coords(1:3,2)/0.5d0, 0.5d0, 0.5d0/
  data coords(1:3,3)/0.4d0, 0.4d0, 0.4d0/

  adp=1.234567890
  na=3
  elements(1) = 'Ca'
  elements(2) = 'Si'
  elements(3) = 'O'
  num = 20
  
  call cmlBeginFile(myfile, 'test.cml')
  call cmlStartCml(myfile)

  ! Add parameter
  call cmlAddParameter(xf=myfile, name='inputSize', value='True')
  call cmlAddParameter(xf=myfile, name='inputSize', value=.True.)
  call cmlAddParameter(xf=myfile, name='inputSize', value=3)
  call cmlAddParameter(xf=myfile, name='inputSize', value=3.0)
  call cmlAddParameter(xf=myfile, name='inputSize', value=3.0d0)
  call cmlAddProperty(xf=myfile, title='inputSize', value='True')
  call cmlAddProperty(xf=myfile, title='inputSize', value=.True.)
  call cmlAddProperty(xf=myfile, title='inputSize', value=3)
  call cmlAddProperty(xf=myfile, title='inputSize', value=3.0)
  call cmlAddProperty(xf=myfile, title='inputSize', value=3.0d0)

  call cmlAddProperty(xf=myfile, title='inputArray', value=(/'one  ', 'two  ', 'three'/))
  call cmlAddProperty(xf=myfile, title='inputArray', value=(/.true., .false./))
  call cmlAddProperty(xf=myfile, title='inputArray', value=(/1, 2/))
  call cmlAddProperty(xf=myfile, title='inputArray', value=(/1.0, 2.0/))
  call cmlAddProperty(xf=myfile, title='inputArray', value=(/1.0d0, 2.0d0/))

  ! Add molecule
  call cmlAddMolecule(xf=myfile, natoms=na,elements=elements,coords=coords)                               
  ! Add molecule output in short style
  call cmlAddMolecule(xf=myfile, natoms=na,elements=elements,coords=coords, style='xyz3')

  ! Add molecule output in short style in user supplied format
  call cmlAddMolecule(xf=myfile, natoms=na,elements=elements,coords=coords, style='xyz3', fmt='r6')

  ! End and Close
  call cmlEndCml(myfile)
  call cmlFinishFile(myfile)
 
end program wcml_example
