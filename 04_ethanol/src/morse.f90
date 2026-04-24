module morse
  implicit none

contains
  !==========================================!
  ! Calculate Morse variables from Cartesian !
  !==========================================!
  subroutine get_morse(xyz,morse,r,a)
    integer::i,j,k
    real::xyz(9,3), dr(3), r(9,9), a
    real,dimension(1:36)::morse

    k = 1
    r = 0.d0
    do i=1,8
       do j=i+1,9
          dr = xyz(i,1:3)-xyz(j,1:3)
          r(i,j) = norm2(dr)
          r(j,i) = r(i,j)
          morse(k) = exp(-r(i,j) / a)
          k = k + 1
       end do
    end do

  end subroutine

  !==============================================!
  ! Partial derivative of rij w.r.t. Cartesian   !
  !==============================================!
  function drdx(flag,xindex,xyz,r)
    integer i,j,flag,xindex,xyzind,matom,m
    real::xyz(9,3),r(9,9)
    real::drdx

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
      i = 2
      j = 3
    elseif (xindex.eq.10) then 
      i = 2
      j = 4
    elseif (xindex.eq.11) then 
      i = 2
      j = 5
    elseif (xindex.eq.12) then 
      i = 2
      j = 6
    elseif (xindex.eq.13) then 
      i = 2
      j = 7
    elseif (xindex.eq.14) then 
      i = 2
      j = 8
    elseif (xindex.eq.15) then 
      i = 2
      j = 9
    elseif (xindex.eq.16) then 
      i = 3
      j = 4
    elseif (xindex.eq.17) then 
      i = 3
      j = 5
    elseif (xindex.eq.18) then 
      i = 3
      j = 6
    elseif (xindex.eq.19) then 
      i = 3
      j = 7
    elseif (xindex.eq.20) then 
      i = 3
      j = 8
    elseif (xindex.eq.21) then 
      i = 3
      j = 9
    elseif (xindex.eq.22) then 
      i = 4
      j = 5
    elseif (xindex.eq.23) then 
      i = 4
      j = 6
    elseif (xindex.eq.24) then 
      i = 4
      j = 7
    elseif (xindex.eq.25) then 
      i = 4
      j = 8
    elseif (xindex.eq.26) then 
      i = 4
      j = 9
    elseif (xindex.eq.27) then 
      i = 5
      j = 6
    elseif (xindex.eq.28) then 
      i = 5
      j = 7
    elseif (xindex.eq.29) then 
      i = 5
      j = 8
    elseif (xindex.eq.30) then 
      i = 5
      j = 9
    elseif (xindex.eq.31) then 
      i = 6
      j = 7
    elseif (xindex.eq.32) then 
      i = 6
      j = 8
    elseif (xindex.eq.33) then 
      i = 6
      j = 9
    elseif (xindex.eq.34) then 
      i = 7
      j = 8
    elseif (xindex.eq.35) then 
      i = 7
      j = 9
    elseif (xindex.eq.36) then 
      i = 8
      j = 9
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
