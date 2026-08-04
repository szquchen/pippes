SUBROUTINE mg411_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!! Note: We stop at degree 8 for now.
!-----------------------------------------------------------------------
integer, parameter :: m=4, m2=m*(m-1), mm=m2*(m-2)
integer :: i0, i1, i2, j0, k0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5, d6, d7
if (size(r).ne.nk*nk) then
 stop 'mg411_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5, d6, d7)
pv = 0
j0 = m ; k0 = j0+1
do i0 = 0, m-1
 call mg411_i0 (mxd, m)
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg411_i1 (mxd, m2)
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     call mg411_ii (mxd, mm)
    endif
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg411_i0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(2) = pv(2)+d(i0,j0)*d(i0,k0)/mu
endif
if (3.le.mxd) then
 pv(11) = pv(11)+d2(i0,j0)*d(i0,k0)/mu
 pv(14) = pv(14)+d(i0,j0)*d2(i0,k0)/mu
endif
if (4.le.mxd) then
 pv(30) = pv(30)+d3(i0,j0)*d(i0,k0)/mu
 pv(37) = pv(37)+d2(i0,j0)*d2(i0,k0)/mu
 pv(40) = pv(40)+d(i0,j0)*d3(i0,k0)/mu
endif
END SUBROUTINE mg411_i0
SUBROUTINE mg411_i1 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
 pv(1) = pv(1)+d(i0,i1)*d(i0,k0)/mu
endif
if (3.le.mxd) then
 pv(4) = pv(4)+d2(i0,i1)*d(i0,j0)/mu
 pv(6) = pv(6)+d(i0,i1)*d2(i0,j0)/mu
 pv(7) = pv(7)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(8) = pv(8)+d2(i0,i1)*d(i0,k0)/mu
 pv(10) = pv(10)+d(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(12) = pv(12)+d(i0,i1)*d(i1,j0)*d(i0,k0)/mu
 pv(13) = pv(13)+d(i0,i1)*d2(i0,k0)/mu
 pv(15) = pv(15)+d(i0,i1)*d(i0,k0)*d(i1,k0)/mu
endif
if (4.le.mxd) then
 pv(16) = pv(16)+d4(i0,i1)/mu
 pv(17) = pv(17)+d3(i0,i1)*d(i0,j0)/mu
 pv(20) = pv(20)+d2(i0,i1)*d2(i0,j0)/mu
 pv(22) = pv(22)+d(i0,i1)*d3(i0,j0)/mu
 pv(23) = pv(23)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(24) = pv(24)+d3(i0,i1)*d(i0,k0)/mu
 pv(27) = pv(27)+d2(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(29) = pv(29)+d(i0,i1)*d2(i0,j0)*d(i0,k0)/mu
 pv(31) = pv(31)+d2(i0,i1)*d(i1,j0)*d(i0,k0)/mu
 pv(33) = pv(33)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(34) = pv(34)+d2(i0,i1)*d2(i0,k0)/mu
 pv(36) = pv(36)+d(i0,i1)*d(i0,j0)*d2(i0,k0)/mu
 pv(38) = pv(38)+d(i0,i1)*d(i1,j0)*d2(i0,k0)/mu
 pv(39) = pv(39)+d(i0,i1)*d3(i0,k0)/mu
 pv(41) = pv(41)+d2(i0,i1)*d(i0,k0)*d(i1,k0)/mu
endif
if (5.le.mxd) then
 pv(42) = pv(42)+d5(i0,i1)/mu
 pv(43) = pv(43)+d4(i0,i1)*d(i0,j0)/mu
 pv(45) = pv(45)+d3(i0,i1)*d2(i0,j0)/mu
 pv(47) = pv(47)+d2(i0,i1)*d3(i0,j0)/mu
 pv(48) = pv(48)+d(i0,i1)*d3(i0,j0)*d(i1,j0)/mu
 pv(49) = pv(49)+d4(i0,i1)*d(i0,k0)/mu
 pv(51) = pv(51)+d3(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(53) = pv(53)+d2(i0,i1)*d2(i0,j0)*d(i0,k0)/mu
 pv(55) = pv(55)+d2(i0,i1)*d(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(56) = pv(56)+d(i0,i1)*d2(i0,j0)*d(i1,j0)*d(i0,k0)/mu
 pv(57) = pv(57)+d3(i0,i1)*d2(i0,k0)/mu
 pv(59) = pv(59)+d2(i0,i1)*d(i0,j0)*d2(i0,k0)/mu
 pv(60) = pv(60)+d2(i0,i1)*d(i1,j0)*d2(i0,k0)/mu
 pv(61) = pv(61)+d(i0,i1)*d(i0,j0)*d(i1,j0)*d2(i0,k0)/mu
 pv(62) = pv(62)+d2(i0,i1)*d3(i0,k0)/mu
 pv(63) = pv(63)+d(i0,i1)*d(i1,j0)*d3(i0,k0)/mu
 pv(64) = pv(64)+d(i0,i1)*d3(i0,k0)*d(i1,k0)/mu
endif
END SUBROUTINE mg411_i1
SUBROUTINE mg411_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(3) = pv(3)+d2(i0,i1)*d(i0,i2)/mu
 pv(5) = pv(5)+d(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(9) = pv(9)+d(i0,i1)*d(i0,i2)*d(i0,k0)/mu
endif
if (4.le.mxd) then
 pv(18) = pv(18)+d2(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(19) = pv(19)+d2(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(21) = pv(21)+d(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(25) = pv(25)+d2(i0,i1)*d(i0,i2)*d(i0,k0)/mu
 pv(26) = pv(26)+d2(i0,i1)*d(i1,i2)*d(i0,k0)/mu
 pv(28) = pv(28)+d(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)/mu
 pv(32) = pv(32)+d(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(35) = pv(35)+d(i0,i1)*d(i0,i2)*d2(i0,k0)/mu
endif
if (5.le.mxd) then
 pv(44) = pv(44)+d3(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(46) = pv(46)+d2(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
 pv(50) = pv(50)+d3(i0,i1)*d(i0,i2)*d(i0,k0)/mu
 pv(52) = pv(52)+d2(i0,i1)*d(i0,i2)*d(i0,j0)*d(i0,k0)/mu
 pv(54) = pv(54)+d2(i0,i1)*d(i0,i2)*d(i1,j0)*d(i0,k0)/mu
 pv(58) = pv(58)+d2(i0,i1)*d(i0,i2)*d2(i0,k0)/mu
endif
if (6.le.mxd) then
 pv(65) = pv(65)+d2(i0,i1)*d2(i0,i2)*d(i1,j0)*d(i0,k0)/mu
endif
END SUBROUTINE mg411_ii
END SUBROUTINE mg411_isecs
