SUBROUTINE bg72_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We don't have the irreducible secondaries at degree 9; Magma
!! ran out of memory (4GB max) computing pv(50:62).
!-----------------------------------------------------------------------
integer, parameter :: m=7, n=2, npv=63, &
  mn=m*n, m2n=m*(m-1)*n, mn2=m*n*(n-1), m2n2=m2n*(n-1)
integer :: i0, i1, j0, j1
real (kind=wp) :: pv(0:npv-1), d(0:m-1,0:n-1), d2(0:m-1,0:n-1), &
  d3(0:m-1,0:n-1), d4(0:m-1,0:n-1), d5(0:m-1,0:n-1), &
  d6(0:m-1,0:n-1), d7(0:m-1,0:n-1), d8(0:m-1,0:n-1)
if (size(x).ne.bg72_nr.or.size(v).ne.bg72_nsc(mxd)) then
 stop 'bg72_secs: bad dimensions'
endif
call bg_setd ()
pv = 0
do j0 = 0, n-1
 do i0 = 0, m-1
  if (5.le.mxd) then
   call bg72_deg5_i0j0 ()
  endif
  if (7.le.mxd) then
   call bg72_deg7_i0j0 ()
  endif
  if (8.le.mxd) then
   call bg72_deg8_i0j0 ()
  endif
  do i1 = 0, m-1
  if (i1.ne.i0) then
   if (3.le.mxd) then
    call bg72_deg3_i1j0 ()
   endif
   if (4.le.mxd) then
    call bg72_deg4_i1j0 ()
   endif
   if (5.le.mxd) then
    call bg72_deg5_i1j0 ()
   endif
   if (6.le.mxd) then
    call bg72_deg6_i1j0 ()
   endif
   if (7.le.mxd) then
    call bg72_deg7_i1j0 ()
   endif
   if (8.le.mxd) then
    call bg72_deg8_i1j0 ()
   endif
  endif
  enddo
 enddo
 do j1 = 0, n-1
 if (j1.ne.j0) then
  do i0 = 0, m-1
   if (4.le.mxd) then
    call bg72_deg4_i0j1 ()
   endif
   if (5.le.mxd) then
    call bg72_deg5_i0j1 ()
   endif
   if (6.le.mxd) then
    call bg72_deg6_i0j1 ()
   endif
   if (7.le.mxd) then
    call bg72_deg7_i0j1 ()
   endif
   if (8.le.mxd) then
    call bg72_deg8_i0j1 ()
   endif
   do i1 = 0, m-1
   if (i1.ne.i0) then
    if (4.le.mxd) then
     call bg72_deg4_i1j1 ()
    endif
    if (5.le.mxd) then
     call bg72_deg5_i1j1 ()
    endif
    if (6.le.mxd) then
     call bg72_deg6_i1j1 ()
    endif
    if (7.le.mxd) then
     call bg72_deg7_i1j1 ()
    endif
    if (8.le.mxd) then
     call bg72_deg8_i1j1 ()
    endif
   endif
   enddo
  enddo
 endif
 enddo
enddo
v(0) = 1
if (3.le.mxd) then
 v(1:1) = pv(0:0)
endif
if (4.le.mxd) then
 v(2:5) = pv(1:4)
endif
if (5.le.mxd) then
 v(6:11) = pv(5:10)
endif
if (6.le.mxd) then
 v(12:21) = pv(11:20)
endif
if (7.le.mxd) then
 v(22) = pv(0)*pv(2)
 v(23) = pv(0)*pv(4)
 v(24:36) = pv(21:33)
endif
if (8.le.mxd) then
 v(37) = pv(2)*pv(2)
 v(38) = pv(1)*pv(4)
 v(39) = pv(2)*pv(4)
 v(40) = pv(3)*pv(4)
 v(41) = pv(4)*pv(4)
 v(42) = pv(0)*pv(6)
 v(43) = pv(0)*pv(7)
 v(44) = pv(0)*pv(9)
 v(45) = pv(0)*pv(10)
 v(46:61) = pv(34:49)
endif
if (9.le.mxd) then
 v(62) = pv(4)*pv(5)
 v(63) = pv(2)*pv(6)
 v(64) = pv(4)*pv(6)
 v(65) = pv(1)*pv(7)
 v(66) = pv(2)*pv(7)
 v(67) = pv(3)*pv(7)
 v(68) = pv(4)*pv(7)
 v(69) = pv(4)*pv(8)
 v(70) = pv(2)*pv(9)
 v(71) = pv(4)*pv(9)
 v(72) = pv(1)*pv(10)
 v(73) = pv(2)*pv(10)
 v(74) = pv(3)*pv(10)
 v(75) = pv(4)*pv(10)
 v(76) = pv(0)*pv(12)
 v(77) = pv(0)*pv(13)
 v(78) = pv(0)*pv(15)
 v(79) = pv(0)*pv(16)
 v(80) = pv(0)*pv(17)
 v(81) = pv(0)*pv(19)
 v(82) = pv(0)*pv(20)
 v(83:95) = pv(50:62)
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
   d8(i,j) = x(k)**8
   k = k+1
  enddo
 enddo
END SUBROUTINE bg_setd
SUBROUTINE bg72_deg3_i1j0 ()
 pv(0) = pv(0)+d2(i0,j0)*d(i1,j0)/m2n
END SUBROUTINE bg72_deg3_i1j0
SUBROUTINE bg72_deg4_i1j0 ()
 pv(1) = pv(1)+d3(i0,j0)*d(i1,j0)/m2n
 pv(2) = pv(2)+d2(i0,j0)*d2(i1,j0)/m2n
END SUBROUTINE bg72_deg4_i1j0
SUBROUTINE bg72_deg4_i0j1 ()
 pv(3) = pv(3)+d3(i0,j0)*d(i0,j1)/mn2
END SUBROUTINE bg72_deg4_i0j1
SUBROUTINE bg72_deg4_i1j1 ()
 pv(4) = pv(4)+d2(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
END SUBROUTINE bg72_deg4_i1j1
SUBROUTINE bg72_deg5_i0j0 ()
 pv(5) = pv(5)+d5(i0,j0)/mn
END SUBROUTINE bg72_deg5_i0j0
SUBROUTINE bg72_deg5_i1j0 ()
 pv(6) = pv(6)+d4(i0,j0)*d(i1,j0)/m2n
 pv(7) = pv(7)+d3(i0,j0)*d2(i1,j0)/m2n
END SUBROUTINE bg72_deg5_i1j0
SUBROUTINE bg72_deg5_i0j1 ()
 pv(8) = pv(8)+d4(i0,j0)*d(i0,j1)/mn2
END SUBROUTINE bg72_deg5_i0j1
SUBROUTINE bg72_deg5_i1j1 ()
 pv(9) = pv(9)+d3(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(10) = pv(10)+d2(i0,j0)*d2(i1,j0)*d(i0,j1)/m2n2
END SUBROUTINE bg72_deg5_i1j1
SUBROUTINE bg72_deg6_i1j0 ()
 pv(11) = pv(11)+d5(i0,j0)*d(i1,j0)/m2n
 pv(12) = pv(12)+d4(i0,j0)*d2(i1,j0)/m2n
 pv(13) = pv(13)+d3(i0,j0)*d3(i1,j0)/m2n
END SUBROUTINE bg72_deg6_i1j0
SUBROUTINE bg72_deg6_i0j1 ()
 pv(14) = pv(14)+d5(i0,j0)*d(i0,j1)/mn2
 pv(18) = pv(18)+d4(i0,j0)*d2(i0,j1)/mn2
END SUBROUTINE bg72_deg6_i0j1
SUBROUTINE bg72_deg6_i1j1 ()
 pv(15) = pv(15)+d4(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(16) = pv(16)+d3(i0,j0)*d2(i1,j0)*d(i0,j1)/m2n2
 pv(17) = pv(17)+d2(i0,j0)*d3(i1,j0)*d(i0,j1)/m2n2
 pv(19) = pv(19)+d3(i0,j0)*d(i1,j0)*d2(i0,j1)/m2n2
 pv(20) = pv(20)+d2(i0,j0)*d2(i1,j0)*d(i0,j1)*d(i1,j1)/m2n2
END SUBROUTINE bg72_deg6_i1j1
SUBROUTINE bg72_deg7_i0j0 ()
 pv(21) = pv(21)+d7(i0,j0)/mn
END SUBROUTINE bg72_deg7_i0j0
SUBROUTINE bg72_deg7_i1j0 ()
 pv(22) = pv(22)+d6(i0,j0)*d(i1,j0)/m2n
 pv(23) = pv(23)+d5(i0,j0)*d2(i1,j0)/m2n
 pv(24) = pv(24)+d4(i0,j0)*d3(i1,j0)/m2n
END SUBROUTINE bg72_deg7_i1j0
SUBROUTINE bg72_deg7_i0j1 ()
 pv(25) = pv(25)+d6(i0,j0)*d(i0,j1)/mn2
 pv(30) = pv(30)+d5(i0,j0)*d2(i0,j1)/mn2
END SUBROUTINE bg72_deg7_i0j1
SUBROUTINE bg72_deg7_i1j1 ()
 pv(26) = pv(26)+d5(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(27) = pv(27)+d4(i0,j0)*d2(i1,j0)*d(i0,j1)/m2n2
 pv(28) = pv(28)+d3(i0,j0)*d3(i1,j0)*d(i0,j1)/m2n2
 pv(29) = pv(29)+d2(i0,j0)*d4(i1,j0)*d(i0,j1)/m2n2
 pv(31) = pv(31)+d4(i0,j0)*d(i1,j0)*d2(i0,j1)/m2n2
 pv(32) = pv(32)+d3(i0,j0)*d2(i1,j0)*d2(i0,j1)/m2n2
 pv(33) = pv(33)+d3(i0,j0)*d2(i1,j0)*d(i0,j1)*d(i1,j1)/m2n2
END SUBROUTINE bg72_deg7_i1j1
SUBROUTINE bg72_deg8_i0j0 ()
 pv(34) = pv(34)+d8(i0,j0)/mn
END SUBROUTINE bg72_deg8_i0j0
SUBROUTINE bg72_deg8_i1j0 ()
 pv(35) = pv(35)+d7(i0,j0)*d(i1,j0)/m2n
 pv(36) = pv(36)+d6(i0,j0)*d2(i1,j0)/m2n
 pv(37) = pv(37)+d5(i0,j0)*d3(i1,j0)/m2n
END SUBROUTINE bg72_deg8_i1j0
SUBROUTINE bg72_deg8_i0j1 ()
 pv(38) = pv(38)+d7(i0,j0)*d(i0,j1)/mn2
 pv(43) = pv(43)+d6(i0,j0)*d2(i0,j1)/mn2
 pv(47) = pv(47)+d5(i0,j0)*d3(i0,j1)/mn2
END SUBROUTINE bg72_deg8_i0j1
SUBROUTINE bg72_deg8_i1j1 ()
 pv(39) = pv(39)+d6(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(40) = pv(40)+d5(i0,j0)*d2(i1,j0)*d(i0,j1)/m2n2
 pv(41) = pv(41)+d4(i0,j0)*d3(i1,j0)*d(i0,j1)/m2n2
 pv(42) = pv(42)+d3(i0,j0)*d4(i1,j0)*d(i0,j1)/m2n2
 pv(44) = pv(44)+d5(i0,j0)*d(i1,j0)*d2(i0,j1)/m2n2
 pv(45) = pv(45)+d4(i0,j0)*d2(i1,j0)*d2(i0,j1)/m2n2
 pv(46) = pv(46)+d3(i0,j0)*d3(i1,j0)*d2(i0,j1)/m2n2
 pv(48) = pv(48)+d4(i0,j0)*d(i1,j0)*d3(i0,j1)/m2n2
 pv(49) = pv(49)+d4(i0,j0)*d2(i1,j0)*d(i0,j1)*d(i1,j1)/m2n2
END SUBROUTINE bg72_deg8_i1j1
END SUBROUTINE bg72_secs
