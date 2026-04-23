module pes_shell
use basis_dft
use basis_delta
use morse
implicit none

  real::coeff_dft(1:8040)
  real::coeff_delta(1:569)

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
  function nma_pot(xyz)
    real,dimension(:,:),intent(in)::xyz
    real::nma_pot
    !::::::::::::::::::::::::::::::
    real,dimension(66)::morse
    real,dimension(12,12)::r
    real::m_dft(5403), m_delta(595)
    real::p_dft(8040), p_delta(569)
    integer::i,j,k
    real::a ! morse range parameter

    a = 2.d0

    call get_x(xyz, morse, r, a)
    call evmono_dft(morse, m_dft)
    call evmono_delta(morse, m_delta)
    call evpoly_dft(m_dft, p_dft)
    call evpoly_delta(m_delta, p_delta)

    nma_pot = 248.170442116938
    nma_pot = nma_pot + dot_product(coeff_dft, p_dft)
    nma_pot = nma_pot - dot_product(coeff_delta, p_delta)

    return
  end function nma_pot

  !===========================!
  ! function to calculate the !
  ! analytical gradient       !
  !===========================!
  subroutine calc_ef(xyz, pot, force)
    real,dimension(12,3)::xyz,force
    real::pot
    !::::::::::::::::::::::::::::::
    real,dimension(66)::morse
    real,dimension(12,12)::r
    real,dimension(36)::g_dft, g_delta
    real::m_dft(5403), m_delta(595)
    real::p_dft(8040), p_delta(569)
    integer::i,j,k
    real::a ! morse range parameter

    a = 2.d0
    call get_x(xyz, morse, r, a)
    call evmono_dft(morse, m_dft)
    call evmono_delta(morse, m_delta)
    call evpoly_dft(m_dft, p_dft)
    call evpoly_delta(m_delta, p_delta)

    call deriv_rev_dft(coeff_dft, m_dft, p_dft, xyz, r, a, g_dft)
    call deriv_rev_delta(coeff_delta, m_delta, p_delta, xyz, r, a, g_delta)

    do i=1,12
       force(i,1:3) = -g_dft(3*i-2:3*i)+g_delta(3*i-2:3*i)
    end do

    return
  end subroutine calc_ef

end module pes_shell
