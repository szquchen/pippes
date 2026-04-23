module morse
  implicit none

contains
  !==========================================!
  ! Calculate Morse variables from Cartesian !
  !==========================================!
  subroutine get_x(xyz,x,r,a)
    integer :: i,j
    real(kind=8) :: xyz(12,3) ! Cartesian
    real(kind=8) :: r(12,12) ! internuclear distances
    real(kind=8),dimension(1:66)::x ! Morse variables
    real(kind=8) :: a ! range parameter in Morse variable x = exp(-r / a)

    x( 1 ) = sqrt((xyz( 1 , 1 )-xyz( 2 , 1 ))**2+ &
          (xyz(1,2)-xyz(2,2))**2+ &
          (xyz(1,3)-xyz(2,3))**2)
    x( 2 ) = sqrt((xyz( 1 , 1 )-xyz( 3 , 1 ))**2+ &
          (xyz(1,2)-xyz(3,2))**2+ &
          (xyz(1,3)-xyz(3,3))**2)
    x( 3 ) = sqrt((xyz( 1 , 1 )-xyz( 4 , 1 ))**2+ &
          (xyz(1,2)-xyz(4,2))**2+ &
          (xyz(1,3)-xyz(4,3))**2)
    x( 4 ) = sqrt((xyz( 1 , 1 )-xyz( 5 , 1 ))**2+ &
          (xyz(1,2)-xyz(5,2))**2+ &
          (xyz(1,3)-xyz(5,3))**2)
    x( 5 ) = sqrt((xyz( 1 , 1 )-xyz( 6 , 1 ))**2+ &
          (xyz(1,2)-xyz(6,2))**2+ &
          (xyz(1,3)-xyz(6,3))**2)
    x( 6 ) = sqrt((xyz( 1 , 1 )-xyz( 7 , 1 ))**2+ &
          (xyz(1,2)-xyz(7,2))**2+ &
          (xyz(1,3)-xyz(7,3))**2)
    x( 7 ) = sqrt((xyz( 1 , 1 )-xyz( 8 , 1 ))**2+ &
          (xyz(1,2)-xyz(8,2))**2+ &
          (xyz(1,3)-xyz(8,3))**2)
    x( 8 ) = sqrt((xyz( 1 , 1 )-xyz( 9 , 1 ))**2+ &
          (xyz(1,2)-xyz(9,2))**2+ &
          (xyz(1,3)-xyz(9,3))**2)
    x( 9 ) = sqrt((xyz( 1 , 1 )-xyz( 10 , 1 ))**2+ &
          (xyz(1,2)-xyz(10,2))**2+ &
          (xyz(1,3)-xyz(10,3))**2)
    x( 10 ) = sqrt((xyz( 1 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(1,2)-xyz(11,2))**2+ &
          (xyz(1,3)-xyz(11,3))**2)
    x( 11 ) = sqrt((xyz( 1 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(1,2)-xyz(12,2))**2+ &
          (xyz(1,3)-xyz(12,3))**2)
    x( 12 ) = sqrt((xyz( 2 , 1 )-xyz( 3 , 1 ))**2+ &
          (xyz(2,2)-xyz(3,2))**2+ &
          (xyz(2,3)-xyz(3,3))**2)
    x( 13 ) = sqrt((xyz( 2 , 1 )-xyz( 4 , 1 ))**2+ &
          (xyz(2,2)-xyz(4,2))**2+ &
          (xyz(2,3)-xyz(4,3))**2)
    x( 14 ) = sqrt((xyz( 2 , 1 )-xyz( 5 , 1 ))**2+ &
          (xyz(2,2)-xyz(5,2))**2+ &
          (xyz(2,3)-xyz(5,3))**2)
    x( 15 ) = sqrt((xyz( 2 , 1 )-xyz( 6 , 1 ))**2+ &
          (xyz(2,2)-xyz(6,2))**2+ &
          (xyz(2,3)-xyz(6,3))**2)
    x( 16 ) = sqrt((xyz( 2 , 1 )-xyz( 7 , 1 ))**2+ &
          (xyz(2,2)-xyz(7,2))**2+ &
          (xyz(2,3)-xyz(7,3))**2)
    x( 17 ) = sqrt((xyz( 2 , 1 )-xyz( 8 , 1 ))**2+ &
          (xyz(2,2)-xyz(8,2))**2+ &
          (xyz(2,3)-xyz(8,3))**2)
    x( 18 ) = sqrt((xyz( 2 , 1 )-xyz( 9 , 1 ))**2+ &
          (xyz(2,2)-xyz(9,2))**2+ &
          (xyz(2,3)-xyz(9,3))**2)
    x( 19 ) = sqrt((xyz( 2 , 1 )-xyz( 10 , 1 ))**2+ &
          (xyz(2,2)-xyz(10,2))**2+ &
          (xyz(2,3)-xyz(10,3))**2)
    x( 20 ) = sqrt((xyz( 2 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(2,2)-xyz(11,2))**2+ &
          (xyz(2,3)-xyz(11,3))**2)
    x( 21 ) = sqrt((xyz( 2 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(2,2)-xyz(12,2))**2+ &
          (xyz(2,3)-xyz(12,3))**2)
    x( 22 ) = sqrt((xyz( 3 , 1 )-xyz( 4 , 1 ))**2+ &
          (xyz(3,2)-xyz(4,2))**2+ &
          (xyz(3,3)-xyz(4,3))**2)
    x( 23 ) = sqrt((xyz( 3 , 1 )-xyz( 5 , 1 ))**2+ &
          (xyz(3,2)-xyz(5,2))**2+ &
          (xyz(3,3)-xyz(5,3))**2)
    x( 24 ) = sqrt((xyz( 3 , 1 )-xyz( 6 , 1 ))**2+ &
          (xyz(3,2)-xyz(6,2))**2+ &
          (xyz(3,3)-xyz(6,3))**2)
    x( 25 ) = sqrt((xyz( 3 , 1 )-xyz( 7 , 1 ))**2+ &
          (xyz(3,2)-xyz(7,2))**2+ &
          (xyz(3,3)-xyz(7,3))**2)
    x( 26 ) = sqrt((xyz( 3 , 1 )-xyz( 8 , 1 ))**2+ &
          (xyz(3,2)-xyz(8,2))**2+ &
          (xyz(3,3)-xyz(8,3))**2)
    x( 27 ) = sqrt((xyz( 3 , 1 )-xyz( 9 , 1 ))**2+ &
          (xyz(3,2)-xyz(9,2))**2+ &
          (xyz(3,3)-xyz(9,3))**2)
    x( 28 ) = sqrt((xyz( 3 , 1 )-xyz( 10 , 1 ))**2+ &
          (xyz(3,2)-xyz(10,2))**2+ &
          (xyz(3,3)-xyz(10,3))**2)
    x( 29 ) = sqrt((xyz( 3 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(3,2)-xyz(11,2))**2+ &
          (xyz(3,3)-xyz(11,3))**2)
    x( 30 ) = sqrt((xyz( 3 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(3,2)-xyz(12,2))**2+ &
          (xyz(3,3)-xyz(12,3))**2)
    x( 31 ) = sqrt((xyz( 4 , 1 )-xyz( 5 , 1 ))**2+ &
          (xyz(4,2)-xyz(5,2))**2+ &
          (xyz(4,3)-xyz(5,3))**2)
    x( 32 ) = sqrt((xyz( 4 , 1 )-xyz( 6 , 1 ))**2+ &
          (xyz(4,2)-xyz(6,2))**2+ &
          (xyz(4,3)-xyz(6,3))**2)
    x( 33 ) = sqrt((xyz( 4 , 1 )-xyz( 7 , 1 ))**2+ &
          (xyz(4,2)-xyz(7,2))**2+ &
          (xyz(4,3)-xyz(7,3))**2)
    x( 34 ) = sqrt((xyz( 4 , 1 )-xyz( 8 , 1 ))**2+ &
          (xyz(4,2)-xyz(8,2))**2+ &
          (xyz(4,3)-xyz(8,3))**2)
    x( 35 ) = sqrt((xyz( 4 , 1 )-xyz( 9 , 1 ))**2+ &
          (xyz(4,2)-xyz(9,2))**2+ &
          (xyz(4,3)-xyz(9,3))**2)
    x( 36 ) = sqrt((xyz( 4 , 1 )-xyz( 10 , 1 ))**2+ &
          (xyz(4,2)-xyz(10,2))**2+ &
          (xyz(4,3)-xyz(10,3))**2)
    x( 37 ) = sqrt((xyz( 4 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(4,2)-xyz(11,2))**2+ &
          (xyz(4,3)-xyz(11,3))**2)
    x( 38 ) = sqrt((xyz( 4 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(4,2)-xyz(12,2))**2+ &
          (xyz(4,3)-xyz(12,3))**2)
    x( 39 ) = sqrt((xyz( 5 , 1 )-xyz( 6 , 1 ))**2+ &
          (xyz(5,2)-xyz(6,2))**2+ &
          (xyz(5,3)-xyz(6,3))**2)
    x( 40 ) = sqrt((xyz( 5 , 1 )-xyz( 7 , 1 ))**2+ &
          (xyz(5,2)-xyz(7,2))**2+ &
          (xyz(5,3)-xyz(7,3))**2)
    x( 41 ) = sqrt((xyz( 5 , 1 )-xyz( 8 , 1 ))**2+ &
          (xyz(5,2)-xyz(8,2))**2+ &
          (xyz(5,3)-xyz(8,3))**2)
    x( 42 ) = sqrt((xyz( 5 , 1 )-xyz( 9 , 1 ))**2+ &
          (xyz(5,2)-xyz(9,2))**2+ &
          (xyz(5,3)-xyz(9,3))**2)
    x( 43 ) = sqrt((xyz( 5 , 1 )-xyz( 10 , 1 ))**2+ &
          (xyz(5,2)-xyz(10,2))**2+ &
          (xyz(5,3)-xyz(10,3))**2)
    x( 44 ) = sqrt((xyz( 5 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(5,2)-xyz(11,2))**2+ &
          (xyz(5,3)-xyz(11,3))**2)
    x( 45 ) = sqrt((xyz( 5 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(5,2)-xyz(12,2))**2+ &
          (xyz(5,3)-xyz(12,3))**2)
    x( 46 ) = sqrt((xyz( 6 , 1 )-xyz( 7 , 1 ))**2+ &
          (xyz(6,2)-xyz(7,2))**2+ &
          (xyz(6,3)-xyz(7,3))**2)
    x( 47 ) = sqrt((xyz( 6 , 1 )-xyz( 8 , 1 ))**2+ &
          (xyz(6,2)-xyz(8,2))**2+ &
          (xyz(6,3)-xyz(8,3))**2)
    x( 48 ) = sqrt((xyz( 6 , 1 )-xyz( 9 , 1 ))**2+ &
          (xyz(6,2)-xyz(9,2))**2+ &
          (xyz(6,3)-xyz(9,3))**2)
    x( 49 ) = sqrt((xyz( 6 , 1 )-xyz( 10 , 1 ))**2+ &
          (xyz(6,2)-xyz(10,2))**2+ &
          (xyz(6,3)-xyz(10,3))**2)
    x( 50 ) = sqrt((xyz( 6 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(6,2)-xyz(11,2))**2+ &
          (xyz(6,3)-xyz(11,3))**2)
    x( 51 ) = sqrt((xyz( 6 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(6,2)-xyz(12,2))**2+ &
          (xyz(6,3)-xyz(12,3))**2)
    x( 52 ) = sqrt((xyz( 7 , 1 )-xyz( 8 , 1 ))**2+ &
          (xyz(7,2)-xyz(8,2))**2+ &
          (xyz(7,3)-xyz(8,3))**2)
    x( 53 ) = sqrt((xyz( 7 , 1 )-xyz( 9 , 1 ))**2+ &
          (xyz(7,2)-xyz(9,2))**2+ &
          (xyz(7,3)-xyz(9,3))**2)
    x( 54 ) = sqrt((xyz( 7 , 1 )-xyz( 10 , 1 ))**2+ &
          (xyz(7,2)-xyz(10,2))**2+ &
          (xyz(7,3)-xyz(10,3))**2)
    x( 55 ) = sqrt((xyz( 7 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(7,2)-xyz(11,2))**2+ &
          (xyz(7,3)-xyz(11,3))**2)
    x( 56 ) = sqrt((xyz( 7 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(7,2)-xyz(12,2))**2+ &
          (xyz(7,3)-xyz(12,3))**2)
    x( 57 ) = sqrt((xyz( 8 , 1 )-xyz( 9 , 1 ))**2+ &
          (xyz(8,2)-xyz(9,2))**2+ &
          (xyz(8,3)-xyz(9,3))**2)
    x( 58 ) = sqrt((xyz( 8 , 1 )-xyz( 10 , 1 ))**2+ &
          (xyz(8,2)-xyz(10,2))**2+ &
          (xyz(8,3)-xyz(10,3))**2)
    x( 59 ) = sqrt((xyz( 8 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(8,2)-xyz(11,2))**2+ &
          (xyz(8,3)-xyz(11,3))**2)
    x( 60 ) = sqrt((xyz( 8 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(8,2)-xyz(12,2))**2+ &
          (xyz(8,3)-xyz(12,3))**2)
    x( 61 ) = sqrt((xyz( 9 , 1 )-xyz( 10 , 1 ))**2+ &
          (xyz(9,2)-xyz(10,2))**2+ &
          (xyz(9,3)-xyz(10,3))**2)
    x( 62 ) = sqrt((xyz( 9 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(9,2)-xyz(11,2))**2+ &
          (xyz(9,3)-xyz(11,3))**2)
    x( 63 ) = sqrt((xyz( 9 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(9,2)-xyz(12,2))**2+ &
          (xyz(9,3)-xyz(12,3))**2)
    x( 64 ) = sqrt((xyz( 10 , 1 )-xyz( 11 , 1 ))**2+ &
          (xyz(10,2)-xyz(11,2))**2+ &
          (xyz(10,3)-xyz(11,3))**2)
    x( 65 ) = sqrt((xyz( 10 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(10,2)-xyz(12,2))**2+ &
          (xyz(10,3)-xyz(12,3))**2)
    x( 66 ) = sqrt((xyz( 11 , 1 )-xyz( 12 , 1 ))**2+ &
          (xyz(11,2)-xyz(12,2))**2+ &
          (xyz(11,3)-xyz(12,3))**2)

    do i=1,12
       r(i,i)=0.d0
       do j=i+1,12
          r(i,j)=sqrt( (xyz(i,1)-xyz(j,1))**2 + (xyz(i,2)-xyz(j,2))**2 + &
                       (xyz(i,3)-xyz(j,3))**2 )
          r(j,i)=r(i,j)
       enddo
    enddo

    do i=1,66
       x(i) = exp(-x(i) / a)
    end do

  end subroutine

  !==============================================!
  ! Partial derivative of rij w.r.t. Cartesian   !
  !==============================================!
  function drdx(flag,xindex,xyz,r)
    integer i,j,flag,xindex,xyzind,matom,m
    real(kind=8)::xyz(12,3), drdx
    real(kind=8)::r(12,12)

    if (xindex.eq.1) then 
      i = 1
      j = 2
    elseif (xindex.eq.2) then 
      i = 1
      j = 3
    elseif (xindex.eq.3) then 
      i = 1
      j = 4
    elseif (xindex.eq.4) then 
      i = 1
      j = 5
    elseif (xindex.eq.5) then 
      i = 1
      j = 6
    elseif (xindex.eq.6) then 
      i = 1
      j = 7
    elseif (xindex.eq.7) then 
      i = 1
      j = 8
    elseif (xindex.eq.8) then 
      i = 1
      j = 9
    elseif (xindex.eq.9) then 
      i = 1
      j = 10
    elseif (xindex.eq.10) then 
      i = 1
      j = 11
    elseif (xindex.eq.11) then 
      i = 1
      j = 12
    elseif (xindex.eq.12) then 
      i = 2
      j = 3
    elseif (xindex.eq.13) then 
      i = 2
      j = 4
    elseif (xindex.eq.14) then 
      i = 2
      j = 5
    elseif (xindex.eq.15) then 
      i = 2
      j = 6
    elseif (xindex.eq.16) then 
      i = 2
      j = 7
    elseif (xindex.eq.17) then 
      i = 2
      j = 8
    elseif (xindex.eq.18) then 
      i = 2
      j = 9
    elseif (xindex.eq.19) then 
      i = 2
      j = 10
    elseif (xindex.eq.20) then 
      i = 2
      j = 11
    elseif (xindex.eq.21) then 
      i = 2
      j = 12
    elseif (xindex.eq.22) then 
      i = 3
      j = 4
    elseif (xindex.eq.23) then 
      i = 3
      j = 5
    elseif (xindex.eq.24) then 
      i = 3
      j = 6
    elseif (xindex.eq.25) then 
      i = 3
      j = 7
    elseif (xindex.eq.26) then 
      i = 3
      j = 8
    elseif (xindex.eq.27) then 
      i = 3
      j = 9
    elseif (xindex.eq.28) then 
      i = 3
      j = 10
    elseif (xindex.eq.29) then 
      i = 3
      j = 11
    elseif (xindex.eq.30) then 
      i = 3
      j = 12
    elseif (xindex.eq.31) then 
      i = 4
      j = 5
    elseif (xindex.eq.32) then 
      i = 4
      j = 6
    elseif (xindex.eq.33) then 
      i = 4
      j = 7
    elseif (xindex.eq.34) then 
      i = 4
      j = 8
    elseif (xindex.eq.35) then 
      i = 4
      j = 9
    elseif (xindex.eq.36) then 
      i = 4
      j = 10
    elseif (xindex.eq.37) then 
      i = 4
      j = 11
    elseif (xindex.eq.38) then 
      i = 4
      j = 12
    elseif (xindex.eq.39) then 
      i = 5
      j = 6
    elseif (xindex.eq.40) then 
      i = 5
      j = 7
    elseif (xindex.eq.41) then 
      i = 5
      j = 8
    elseif (xindex.eq.42) then 
      i = 5
      j = 9
    elseif (xindex.eq.43) then 
      i = 5
      j = 10
    elseif (xindex.eq.44) then 
      i = 5
      j = 11
    elseif (xindex.eq.45) then 
      i = 5
      j = 12
    elseif (xindex.eq.46) then 
      i = 6
      j = 7
    elseif (xindex.eq.47) then 
      i = 6
      j = 8
    elseif (xindex.eq.48) then 
      i = 6
      j = 9
    elseif (xindex.eq.49) then 
      i = 6
      j = 10
    elseif (xindex.eq.50) then 
      i = 6
      j = 11
    elseif (xindex.eq.51) then 
      i = 6
      j = 12
    elseif (xindex.eq.52) then 
      i = 7
      j = 8
    elseif (xindex.eq.53) then 
      i = 7
      j = 9
    elseif (xindex.eq.54) then 
      i = 7
      j = 10
    elseif (xindex.eq.55) then 
      i = 7
      j = 11
    elseif (xindex.eq.56) then 
      i = 7
      j = 12
    elseif (xindex.eq.57) then 
      i = 8
      j = 9
    elseif (xindex.eq.58) then 
      i = 8
      j = 10
    elseif (xindex.eq.59) then 
      i = 8
      j = 11
    elseif (xindex.eq.60) then 
      i = 8
      j = 12
    elseif (xindex.eq.61) then 
      i = 9
      j = 10
    elseif (xindex.eq.62) then 
      i = 9
      j = 11
    elseif (xindex.eq.63) then 
      i = 9
      j = 12
    elseif (xindex.eq.64) then 
      i = 10
      j = 11
    elseif (xindex.eq.65) then 
      i = 10
      j = 12
    elseif (xindex.eq.66) then 
      i = 11
      j = 12
    endif

    m=flag
    matom=INT((dble(m)-0.00001d0)/3.d0)+1
    xyzind=MOD(m-1,3)+1

    drdx = 0.d0
    if (matom.eq.i.or.matom.eq.j) then
       drdx=(xyz(i,xyzind)-xyz(j,xyzind))/r(i,j)
       if (matom.eq.j) then
          drdx = -drdx
       endif
    endif

    return
  end function

end module morse
