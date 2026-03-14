module pes_shell
  use bemsa
  implicit none

  real::coef(849)
  character(len=90)::coef_path='/home/quchen/Bowman/projects/pippes/03_ch5p/coeff'

  ! Define the mass of different atoms
  real,parameter::c_mass= 12.0000000  !21874.66
  real,parameter::h_mass=  1.0078250  !1837.15
  real,parameter::d_mass=  2.0141018  !D
  real,parameter::o_mass= 15.9949146  !29156.95
  real,parameter::n_mass= 14.003074
  real,parameter::pi=3.14159265

  ! Define constants
  real,parameter::emass=1822.88848
  real,parameter::auang=0.5291772083
  real,parameter::aucm=219474.6313710

contains
  !==========================!
  ! read in the coefficients !
  !==========================!
  subroutine pes_init()
    integer::i

    open(20,file=trim(coef_path)//'/coeff_CH5+_51_6',status="old")
    do i=1,size(coef)
       read(20,*) coef(i)
    end do

    close(20)
    return
  end subroutine

  !================================!
  ! calculate the potential energy !
  !   x(188888888dinates in bohr !
  !================================!
  function f(x)
    real::x(18),xyz(6,3),morse(15), f
    real::m(14029), p(849)
    integer::i

    do i=1,6
       xyz(i,:) = x(3*i-2:3*i)
    end do
    call get_x(xyz, morse)
    call evmono(morse, m)
    call evpoly(m, p)
    f = dot_product(coef, p)

    return
  end function

  !================================!
  ! calculate the gradients        !
  !   x(3,15): coordinates in bohr !
  !================================!
  function g(x)
    real::x(18), g(18), xyz(6,3)
    real::m(14029), p(849), morse(15)
    integer::i

    do i=1,6
       xyz(i,:) = x(3*i-2:3*i)
    end do
    call get_x(xyz, morse)
    call evmono(morse, m)
    call evpoly(m, p)
    call derivative_reverse(coef,m,p,xyz,g)

    return
  end function

  !=============================!
  ! Hessian using the gradient  !
  !=============================!
  subroutine hessian(x,H)
    real,dimension(:),intent(in)::x
    real,dimension(:,:),intent(inout)::H
    !:::::::::::::::::::::::::::
    real::xyz(6,3)
    integer::i

    do i=1,6
       xyz(i,:) = x(3*i-2:3*i)
    end do
    call hessianrev(coef,xyz,H)

    return
  end subroutine hessian

end module
