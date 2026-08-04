subroutine getrvec (m, r, vec)
implicit none
! version for X4Y2
integer nk, m
parameter (nk=6)
real r(0:nk-1,0:nk-1), vec(0:m-1)
integer i, j
real x(0:2), r1(0:nk-1,0:nk-1), t0, t1
!-----------------------------------------------------------------------
! Test for compatibility
if (.not.(m.eq.1.or.m.eq.4)) then
 stop 'getrvec - wrong dimension'
endif
! Computation
x = 0
do i = 0, nk-1
 do j = 0, nk-1
  if (i.eq.j) then
   r1(i,j) = 0
  else
   r1(i,j) = exp(-r(i,j))/r(i,j)
  endif
 enddo
enddo
! XY distance
x(0) = sum(r1(0:3,4:5))/6
! XX distance
t0 = 0
do i = 0, 3
 do j = i+1, 3
  t0 = t0+r1(i,j)/6
 enddo
enddo
x(1) = t0
! YY distance
x(2) = r1(4,5)
! set vec
vec(0) = 1
if (4.le.m) then
 vec(1:3) = x
endif
return
end subroutine getrvec
