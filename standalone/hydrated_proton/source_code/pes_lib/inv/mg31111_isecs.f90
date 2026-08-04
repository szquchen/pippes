SUBROUTINE mg31111_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!! Note: We stop at degree 6 for now.
!-----------------------------------------------------------------------
integer, parameter :: m=3, mm=m*(m-1)
integer :: i0, i1, j0, k0, l0, m0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2
if (size(r).ne.nk*nk) then
 stop 'mg31111_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2)
pv = 0
j0 = m ; k0 = j0+1 ; l0 = k0+1 ; m0 = l0+1
do i0 = 0, m-1
 call mg31111_i0 (mxd, m)
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg31111_ii (mxd, mm)
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg31111_i0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(2) = pv(2)+d(i0,j0)*d(i0,k0)/mu
 pv(4) = pv(4)+d(i0,j0)*d(i0,l0)/mu
 pv(5) = pv(5)+d(i0,k0)*d(i0,l0)/mu
 pv(7) = pv(7)+d(i0,j0)*d(i0,m0)/mu
 pv(8) = pv(8)+d(i0,k0)*d(i0,m0)/mu
 pv(9) = pv(9)+d(i0,l0)*d(i0,m0)/mu
endif
if (3.le.mxd) then
 pv(14) = pv(14)+d2(i0,j0)*d(i0,k0)/mu
 pv(16) = pv(16)+d(i0,j0)*d2(i0,k0)/mu
 pv(19) = pv(19)+d2(i0,j0)*d(i0,l0)/mu
 pv(21) = pv(21)+d(i0,j0)*d(i0,k0)*d(i0,l0)/mu
 pv(22) = pv(22)+d2(i0,k0)*d(i0,l0)/mu
 pv(24) = pv(24)+d(i0,j0)*d2(i0,l0)/mu
 pv(25) = pv(25)+d(i0,k0)*d2(i0,l0)/mu
 pv(28) = pv(28)+d2(i0,j0)*d(i0,m0)/mu
 pv(30) = pv(30)+d(i0,j0)*d(i0,k0)*d(i0,m0)/mu
 pv(31) = pv(31)+d2(i0,k0)*d(i0,m0)/mu
 pv(33) = pv(33)+d(i0,j0)*d(i0,l0)*d(i0,m0)/mu
 pv(34) = pv(34)+d(i0,k0)*d(i0,l0)*d(i0,m0)/mu
 pv(35) = pv(35)+d2(i0,l0)*d(i0,m0)/mu
 pv(37) = pv(37)+d(i0,j0)*d2(i0,m0)/mu
 pv(38) = pv(38)+d(i0,k0)*d2(i0,m0)/mu
 pv(39) = pv(39)+d(i0,l0)*d2(i0,m0)/mu
endif
END SUBROUTINE mg31111_i0
SUBROUTINE mg31111_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
 pv(1) = pv(1)+d(i0,i1)*d(i0,k0)/mu
 pv(3) = pv(3)+d(i0,i1)*d(i0,l0)/mu
 pv(6) = pv(6)+d(i0,i1)*d(i0,m0)/mu
endif
if (3.le.mxd) then
 pv(10) = pv(10)+d2(i0,i1)*d(i0,j0)/mu
 pv(11) = pv(11)+d(i0,i1)*d2(i0,j0)/mu
 pv(12) = pv(12)+d2(i0,i1)*d(i0,k0)/mu
 pv(13) = pv(13)+d(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(15) = pv(15)+d(i0,i1)*d2(i0,k0)/mu
 pv(17) = pv(17)+d2(i0,i1)*d(i0,l0)/mu
 pv(18) = pv(18)+d(i0,i1)*d(i0,j0)*d(i0,l0)/mu
 pv(20) = pv(20)+d(i0,i1)*d(i0,k0)*d(i0,l0)/mu
 pv(23) = pv(23)+d(i0,i1)*d2(i0,l0)/mu
 pv(26) = pv(26)+d2(i0,i1)*d(i0,m0)/mu
 pv(27) = pv(27)+d(i0,i1)*d(i0,j0)*d(i0,m0)/mu
 pv(29) = pv(29)+d(i0,i1)*d(i0,k0)*d(i0,m0)/mu
 pv(32) = pv(32)+d(i0,i1)*d(i0,l0)*d(i0,m0)/mu
 pv(36) = pv(36)+d(i0,i1)*d2(i0,m0)/mu
endif
END SUBROUTINE mg31111_ii
END SUBROUTINE mg31111_isecs
