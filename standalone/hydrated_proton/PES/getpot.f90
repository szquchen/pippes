program main
  use pes_shell
  implicit none
  real,dimension(:,:),allocatable::x
  character,dimension(:),allocatable::symb
  character(len=128)::filename
  integer::natm
  real :: pot,dm(3)
  integer :: i
  
  open(21,file='h9o4.xyz') !file of cartesian coordinates
  natm=13  !number of atoms
  nw=(natm-4)/ 3
 
  
  !initial the PES and DMS
  call pes_init(1,nw)

  allocate(x(3,natm))
  allocate(symb(natm))
  read(21,*) natm
  read(21,*)
  do i=1,natm
     read(21,*) symb(i),x(1:3,i)
  end do
  x=x/auang  
  pot = f(x)
  dm=dp(x)
 
  print*, 'Total potential energy  = ', pot , ' Hartree'
  print*, 'Dipole moment  = ', dm , ' Hartree'
  
end program main
