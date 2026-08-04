SUBROUTINE bg42_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=4, n=2, npv=13, &
  mn=m*n, m2n=m*(m-1)*n, mn2=m*n*(n-1), m2n2=m2n*(n-1)
integer :: i0, i1, j0, j1
real (kind=wp) :: pv(0:npv-1), d(0:m-1,0:n-1), d2(0:m-1,0:n-1), &
  d3(0:m-1,0:n-1), d4(0:m-1,0:n-1), d5(0:m-1,0:n-1), &
  d6(0:m-1,0:n-1), d7(0:m-1,0:n-1)
if (size(x).ne.bg42_nr.or.size(v).ne.bg42_nsc(mxd)) then
 stop 'bg42_secs: bad dimensions'
endif
call bg_setd ()
pv = 0
do j0 = 0, n-1
 do i0 = 0, m-1
  if (3.le.mxd) then
   call bg42_deg3_i0j0 ()
  endif
  if (5.le.mxd) then
   call bg42_deg5_i0j0 ()
  endif
  if (7.le.mxd) then
   call bg42_deg7_i0j0 ()
  endif
  do i1 = 0, m-1
  if (i1.ne.i0) then
   if (3.le.mxd) then
    call bg42_deg3_i1j0 ()
   endif
   if (4.le.mxd) then
    call bg42_deg4_i1j0 ()
   endif
   if (5.le.mxd) then
    call bg42_deg5_i1j0 ()
   endif
   if (6.le.mxd) then
    call bg42_deg6_i1j0 ()
   endif
  endif
  enddo
 enddo
 do j1 = 0, n-1
 if (j1.ne.j0) then
  do i0 = 0, m-1
   if (4.le.mxd) then
    call bg42_deg4_i0j1 ()
   endif
   if (5.le.mxd) then
    call bg42_deg5_i0j1 ()
   endif
   if (6.le.mxd) then
    call bg42_deg6_i0j1 ()
   endif
   do i1 = 0, m-1
   if (i1.ne.i0) then
    if (4.le.mxd) then
     call bg42_deg4_i1j1 ()
    endif
    if (5.le.mxd) then
     call bg42_deg5_i1j1 ()
    endif
   endif
   enddo
  enddo
 endif
 enddo
enddo
v(0) = 1
if (3.le.mxd) then
 v(1:2) = pv(0:1)
endif
if (4.le.mxd) then
 v(3:6) = pv(2:5)
endif
if (5.le.mxd) then
 v(7:10) = pv(6:9)
endif
if (6.le.mxd) then
 v(11) = pv(0)*pv(1)
 v(12) = pv(1)*pv(1)
 v(13:14) = pv(10:11)
endif
if (7.le.mxd) then
 v(15) = pv(0)*pv(3)
 v(16) = pv(1)*pv(3)
 v(17) = pv(1)*pv(4)
 v(18) = pv(0)*pv(5)
 v(19) = pv(1)*pv(5)
 v(20:20) = pv(12:12)
endif
if (8.le.mxd) then
 v(21) = pv(3)*pv(5)
 v(22) = pv(0)*pv(7)
 v(23) = pv(1)*pv(7)
 v(24) = pv(1)*pv(8)
 v(25) = pv(0)*pv(9)
 v(26) = pv(1)*pv(9)
endif
if (9.le.mxd) then
 v(27) = pv(4)*pv(9)
 v(28) = pv(5)*pv(9)
 v(29) = pv(0)*pv(10)
 v(30) = pv(1)*pv(10)
 v(31) = pv(0)*pv(11)
 v(32) = pv(1)*pv(11)
endif
return
CONTAINS
SUBROUTINE bg_setd ()
 integer :: i, j, k
 k = 0
 do j = 0, n-1
  do i = 0, m-1
   d(i,j) = x(k)
   d2(i,j) = x(k)**2
   d3(i,j) = x(k)**3
   d4(i,j) = x(k)**4
   d5(i,j) = x(k)**5
   d6(i,j) = x(k)**6
   d7(i,j) = x(k)**7
   k = k+1
  enddo
 enddo
END SUBROUTINE bg_setd
SUBROUTINE bg42_deg3_i0j0 ()
 pv(0) = pv(0)+d3(i0,j0)/mn
END SUBROUTINE bg42_deg3_i0j0
SUBROUTINE bg42_deg3_i1j0 ()
 pv(1) = pv(1)+d2(i0,j0)*d(i1,j0)/m2n
END SUBROUTINE bg42_deg3_i1j0
SUBROUTINE bg42_deg4_i1j0 ()
 pv(2) = pv(2)+d3(i0,j0)*d(i1,j0)/m2n
 pv(3) = pv(3)+d2(i0,j0)*d2(i1,j0)/m2n
END SUBROUTINE bg42_deg4_i1j0
SUBROUTINE bg42_deg4_i0j1 ()
 pv(4) = pv(4)+d3(i0,j0)*d(i0,j1)/mn2
END SUBROUTINE bg42_deg4_i0j1
SUBROUTINE bg42_deg4_i1j1 ()
 pv(5) = pv(5)+d2(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
END SUBROUTINE bg42_deg4_i1j1
SUBROUTINE bg42_deg5_i0j0 ()
 pv(6) = pv(6)+d5(i0,j0)/mn
END SUBROUTINE bg42_deg5_i0j0
SUBROUTINE bg42_deg5_i1j0 ()
 pv(7) = pv(7)+d4(i0,j0)*d(i1,j0)/m2n
END SUBROUTINE bg42_deg5_i1j0
SUBROUTINE bg42_deg5_i0j1 ()
 pv(8) = pv(8)+d4(i0,j0)*d(i0,j1)/mn2
END SUBROUTINE bg42_deg5_i0j1
SUBROUTINE bg42_deg5_i1j1 ()
 pv(9) = pv(9)+d3(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
END SUBROUTINE bg42_deg5_i1j1
SUBROUTINE bg42_deg6_i1j0 ()
 pv(10) = pv(10)+d4(i0,j0)*d2(i1,j0)/m2n
END SUBROUTINE bg42_deg6_i1j0
SUBROUTINE bg42_deg6_i0j1 ()
 pv(11) = pv(11)+d5(i0,j0)*d(i0,j1)/mn2
END SUBROUTINE bg42_deg6_i0j1
SUBROUTINE bg42_deg7_i0j0 ()
 pv(12) = pv(12)+d7(i0,j0)/mn
END SUBROUTINE bg42_deg7_i0j0
END SUBROUTINE bg42_secs
