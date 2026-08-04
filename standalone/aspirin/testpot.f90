program main
use pes_shell
implicit none

  real :: v
  logical :: havegrad
  character (len=2) :: symb
  integer :: i, natm
  real, dimension (:), allocatable :: x
  real, dimension (:), allocatable :: g
  character(len=32)::fname, bname

  call getarg(1,fname)
  i=index(fname,'.',.true.)
  bname=fname(1:i-1)

  open(12,file=trim(fname),status='old')
  open(13,file=trim(bname)//'.out',status='unknown')

  call pes_init()
  read (12,*) natm
  read (12,*) 

  allocate(x(3*natm),g(3*natm))

  do i=1,natm
     read (12,*)symb, x(3*i-2:3*i)
  end do
  x=x/auang

  v=pot(x)
  g=grad(x)
  write(13,*) "Potential (hartree): ", v
  write(13,*) "Gradient (hartree/bohr): "
  do i=1,natm
     write(13,*) g(3*i-2:3*i)
  end do

  deallocate(x)
  close (12)
  close (13)

end program
