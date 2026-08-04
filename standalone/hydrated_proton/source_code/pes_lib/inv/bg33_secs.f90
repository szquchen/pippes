SUBROUTINE bg33_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We don't have the secondaries at degree 9; Magma ran out of
!! memory (4GB max) computing v(68:89).
!-----------------------------------------------------------------------
integer, parameter :: m=3, n=3, npv=28, &
  mn=m*n, m2n=m*(m-1)*n, mn2=m*n*(n-1), m2n2=m2n*(n-1)
integer :: i0, i1, j0, j1
real (kind=wp) :: pv(0:npv-1), d(0:m-1,0:n-1), d2(0:m-1,0:n-1), &
  d3(0:m-1,0:n-1), d4(0:m-1,0:n-1), d5(0:m-1,0:n-1), &
  d6(0:m-1,0:n-1), d7(0:m-1,0:n-1)
if (size(x).ne.bg33_nr.or.size(v).ne.bg33_nsc(mxd)) then
 stop 'bg33_secs: bad dimensions'
endif
call bg_setd ()
pv = 0
do j0 = 0, n-1
 do i0 = 0, m-1
  if (5.le.mxd) then
   call bg33_deg5_i0j0 ()
  endif
  if (7.le.mxd) then
   call bg33_deg7_i0j0 ()
  endif
  do i1 = 0, m-1
  if (i1.ne.i0) then
   if (3.le.mxd) then
    call bg33_deg3_i1j0 ()
   endif
   if (4.le.mxd) then
    call bg33_deg4_i1j0 ()
   endif
   if (5.le.mxd) then
    call bg33_deg5_i1j0 ()
   endif
   if (6.le.mxd) then
    call bg33_deg6_i1j0 ()
   endif
   if (7.le.mxd) then
    call bg33_deg7_i1j0 ()
   endif
  endif
  enddo
 enddo
 do j1 = 0, n-1
 if (j1.ne.j0) then
  do i0 = 0, m-1
   if (3.le.mxd) then
    call bg33_deg3_i0j1 ()
   endif
   if (4.le.mxd) then
    call bg33_deg4_i0j1 ()
   endif
   if (5.le.mxd) then
    call bg33_deg5_i0j1 ()
   endif
   if (6.le.mxd) then
    call bg33_deg6_i0j1 ()
   endif
   do i1 = 0, m-1
   if (i1.ne.i0) then
    if (3.le.mxd) then
     call bg33_deg3_i1j1 ()
    endif
    if (4.le.mxd) then
     call bg33_deg4_i1j1 ()
    endif
    if (5.le.mxd) then
     call bg33_deg5_i1j1 ()
    endif
    if (6.le.mxd) then
     call bg33_deg6_i1j1 ()
    endif
   endif
   enddo
  enddo
 endif
 enddo
enddo
v(0) = 1
if (3.le.mxd) then
 v(1:3) = pv(0:2)
endif
if (4.le.mxd) then
 v(4:10) = pv(3:9)
endif
if (5.le.mxd) then
 v(11:19) = pv(10:18)
endif
if (6.le.mxd) then
 v(20) = pv(0)*pv(0)
 v(21) = pv(0)*pv(1)
 v(22) = pv(1)*pv(1)
 v(23) = pv(0)*pv(2)
 v(24) = pv(1)*pv(2)
 v(25) = pv(2)*pv(2)
 v(26:32) = pv(19:25)
endif
if (7.le.mxd) then
 v(33) = pv(2)*pv(5)
 v(34) = pv(0)*pv(6)
 v(35) = pv(1)*pv(6)
 v(36) = pv(2)*pv(6)
 v(37) = pv(0)*pv(7)
 v(38) = pv(1)*pv(7)
 v(39) = pv(2)*pv(7)
 v(40) = pv(0)*pv(8)
 v(41) = pv(1)*pv(8)
 v(42) = pv(2)*pv(8)
 v(43) = pv(0)*pv(9)
 v(44) = pv(1)*pv(9)
 v(45) = pv(2)*pv(9)
 v(46:47) = pv(26:27)
endif
if (8.le.mxd) then
 v(48) = pv(8)*pv(9)
 v(49) = pv(9)*pv(9)
 v(50) = pv(0)*pv(13)
 v(51) = pv(1)*pv(13)
 v(52) = pv(2)*pv(13)
 v(53) = pv(0)*pv(14)
 v(54) = pv(1)*pv(14)
 v(55) = pv(2)*pv(14)
 v(56) = pv(0)*pv(15)
 v(57) = pv(1)*pv(15)
 v(58) = pv(2)*pv(15)
 v(59) = pv(0)*pv(16)
 v(60) = pv(1)*pv(16)
 v(61) = pv(2)*pv(16)
 v(62) = pv(0)*pv(17)
 v(63) = pv(1)*pv(17)
 v(64) = pv(2)*pv(17)
 v(65) = pv(0)*pv(18)
 v(66) = pv(1)*pv(18)
 v(67) = pv(2)*pv(18)
endif
if (9.le.mxd) then
 v(68:89) = 0
!! we do not know the secondaries at degree 9
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
SUBROUTINE bg33_deg3_i1j0 ()
 pv(0) = pv(0)+d2(i0,j0)*d(i1,j0)/m2n
END SUBROUTINE bg33_deg3_i1j0
SUBROUTINE bg33_deg3_i0j1 ()
 pv(1) = pv(1)+d2(i0,j0)*d(i0,j1)/mn2
END SUBROUTINE bg33_deg3_i0j1
SUBROUTINE bg33_deg3_i1j1 ()
 pv(2) = pv(2)+d(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
END SUBROUTINE bg33_deg3_i1j1
SUBROUTINE bg33_deg4_i1j0 ()
 pv(3) = pv(3)+d3(i0,j0)*d(i1,j0)/m2n
 pv(4) = pv(4)+d2(i0,j0)*d2(i1,j0)/m2n
END SUBROUTINE bg33_deg4_i1j0
SUBROUTINE bg33_deg4_i0j1 ()
 pv(5) = pv(5)+d3(i0,j0)*d(i0,j1)/mn2
 pv(8) = pv(8)+d2(i0,j0)*d2(i0,j1)/mn2
END SUBROUTINE bg33_deg4_i0j1
SUBROUTINE bg33_deg4_i1j1 ()
 pv(6) = pv(6)+d2(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(7) = pv(7)+d(i0,j0)*d2(i1,j0)*d(i0,j1)/m2n2
 pv(9) = pv(9)+d(i0,j0)*d(i1,j0)*d2(i0,j1)/m2n2
END SUBROUTINE bg33_deg4_i1j1
SUBROUTINE bg33_deg5_i0j0 ()
 pv(10) = pv(10)+d5(i0,j0)/mn
END SUBROUTINE bg33_deg5_i0j0
SUBROUTINE bg33_deg5_i1j0 ()
 pv(11) = pv(11)+d4(i0,j0)*d(i1,j0)/m2n
 pv(12) = pv(12)+d3(i0,j0)*d2(i1,j0)/m2n
END SUBROUTINE bg33_deg5_i1j0
SUBROUTINE bg33_deg5_i0j1 ()
 pv(13) = pv(13)+d4(i0,j0)*d(i0,j1)/mn2
 pv(17) = pv(17)+d3(i0,j0)*d2(i0,j1)/mn2
END SUBROUTINE bg33_deg5_i0j1
SUBROUTINE bg33_deg5_i1j1 ()
 pv(14) = pv(14)+d3(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(15) = pv(15)+d2(i0,j0)*d2(i1,j0)*d(i0,j1)/m2n2
 pv(16) = pv(16)+d(i0,j0)*d3(i1,j0)*d(i0,j1)/m2n2
 pv(18) = pv(18)+d2(i0,j0)*d(i1,j0)*d2(i0,j1)/m2n2
END SUBROUTINE bg33_deg5_i1j1
SUBROUTINE bg33_deg6_i1j0 ()
 pv(19) = pv(19)+d5(i0,j0)*d(i1,j0)/m2n
 pv(20) = pv(20)+d4(i0,j0)*d2(i1,j0)/m2n
END SUBROUTINE bg33_deg6_i1j0
SUBROUTINE bg33_deg6_i0j1 ()
 pv(21) = pv(21)+d5(i0,j0)*d(i0,j1)/mn2
 pv(25) = pv(25)+d4(i0,j0)*d2(i0,j1)/mn2
END SUBROUTINE bg33_deg6_i0j1
SUBROUTINE bg33_deg6_i1j1 ()
 pv(22) = pv(22)+d4(i0,j0)*d(i1,j0)*d(i0,j1)/m2n2
 pv(23) = pv(23)+d3(i0,j0)*d2(i1,j0)*d(i0,j1)/m2n2
 pv(24) = pv(24)+d2(i0,j0)*d3(i1,j0)*d(i0,j1)/m2n2
END SUBROUTINE bg33_deg6_i1j1
SUBROUTINE bg33_deg7_i0j0 ()
 pv(26) = pv(26)+d7(i0,j0)/mn
END SUBROUTINE bg33_deg7_i0j0
SUBROUTINE bg33_deg7_i1j0 ()
 pv(27) = pv(27)+d6(i0,j0)*d(i1,j0)/m2n
END SUBROUTINE bg33_deg7_i1j0
END SUBROUTINE bg33_secs
