module pes_shell
use basis_dft
use basis_delta
implicit none

  real::coeff_dft(1:14752)
  real::coeff_delta(1:208)

contains
  !==================================!
  ! read the coefficients of the PES !
  !==================================!
  subroutine pes_init()
    !::::::::::::::::::
    integer::i

    open(10,file='./coeff/coeff_DFT.dat',status='old')
    do i=1,size(coeff_dft)
       read (10,*) coeff_dft(i)
    end do
    close (10)

    open(10,file='./coeff/coeff_Delta.dat',status='old')
    do i=1,size(coeff_delta)
       read (10,*) coeff_delta(i)
    end do
    close (10)

  end subroutine pes_init

  !====================================!
  ! Function to evaluate the potential !
  !====================================!
  subroutine calcpot(xyz,pot)
    real,dimension(:,:),intent(in)::xyz
    real::pot
    !::::::::::::::::::::::::::::::
    real,dimension(36)::morse
    real,dimension(9,9)::r
    real::m_dft(4848), m_delta(209)
    real::p_dft(14752), p_delta(208)
    integer::i,j,k
    real::a ! morse range parameter

    a = 2.d0

    call get_morse(xyz, morse, r, a)
    call evmono_dft(morse, m_dft)
    call evmono_delta(morse, m_delta)
    call evpoly_dft(m_dft, p_dft)
    call evpoly_delta(m_delta, p_delta)

    pot = 154.842488763961
    pot = pot + dot_product(coeff_dft, p_dft)
    pot = pot - dot_product(coeff_delta, p_delta)

    return
  end subroutine calcpot

  !===========================!
  ! function to calculate the !
  ! analytical gradient       !
  !===========================!
  subroutine calc_ef(xyz, pot, force)
    real,dimension(9,3)::xyz,force
    real::pot
    !::::::::::::::::::::::::::::::
    real,dimension(36)::morse
    real,dimension(9,9)::r
    real,dimension(27)::g_dft, g_delta
    real::m_dft(4848), m_delta(209)
    real::p_dft(14752), p_delta(208)
    integer::i,j,k
    real::a ! morse range parameter

    a = 2.d0
    call get_morse(xyz, morse, r, a)
    call evmono_dft(morse, m_dft)
    call evmono_delta(morse, m_delta)
    call evpoly_dft(m_dft, p_dft)
    call evpoly_delta(m_delta, p_delta)

    pot = 154.842488763961
    pot = pot + dot_product(coeff_dft, p_dft)
    pot = pot - dot_product(coeff_delta, p_delta)

    call deriv_rev_dft(coeff_dft, m_dft, p_dft, xyz, r, a, g_dft)
    call deriv_rev_delta(coeff_delta, m_delta, p_delta, xyz, r, a, g_delta)

    do i=1,9
       force(i,1:3) = -g_dft(3*i-2:3*i)+g_delta(3*i-2:3*i)
    end do

    return
  end subroutine calc_ef

end module pes_shell
