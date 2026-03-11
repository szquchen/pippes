module oxa_shell
use constants
use morse
use bemsa421
implicit none

  real::oxa_coeff(9923) ! number of coefficients

contains
  !==================================!
  ! read the coefficients of the PES !
  !==================================!
  subroutine oxa_init()
    !::::::::::::::::::
    integer::i
    character(len=99)::path

    path = "../drivers/pippes/02_oxa/coeff_421_oxa.dat"
    open(10,file=trim(path),status='old')

    do i=1,size(oxa_coeff)
       read (10,*) oxa_coeff(i)
    end do

    return
    close (10)
  end subroutine oxa_init

  !====================================!
  ! Function to evaluate the potential !
  !====================================!
  function oxa_pot(x)
    real,dimension(:,:),intent(in)::x
    real::oxa_pot
    !::::::::::::::::::::::::::::::
    real,dimension(21)::morse
    real,dimension(7,7)::r
    real::m(31767), p(9923)
    integer::i,j,k

    call get_morse(x, r, morse, 2.0)
    call evmono421(morse, m)
    call evpoly421(m, p)
    oxa_pot = dot_product(p, oxa_coeff) + 5.689024940580545E-007
    
    return
  end function oxa_pot

  !=============================!
  ! Computing energy and force  !
  !=============================!
  subroutine oxa_ef(x,pot,force)
    real,dimension(7,3)::x,force
    real::pot
    !::::::::::::::::::::::::
    real,dimension(21)::morse,gtmp
    real,dimension(7,7)::r
    real::m(31767), p(9923)
    integer::i

    call get_morse(x, r, morse, 2.0)
    call evmono421(morse,m)
    call evpoly421(m,p)
    pot = dot_product(p, oxa_coeff) + 5.689024940580545E-007
    call deriv_rev421(oxa_coeff, m, p, transpose(x), r, gtmp)

    do i=1,7
       force(i,1:3) = -gtmp(3*i-2:3*i)
    end do

    return
  end subroutine

end module oxa_shell
