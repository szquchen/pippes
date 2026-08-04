SUBROUTINE mg22111_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, n=2, mm=m, nn=n
integer :: i0, j0, k0, l0, m0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3
if (size(r).ne.nk*nk) then
 stop 'mg22111_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3)
pv = 0
k0 = m+n ; l0 = k0+1 ; m0 = l0+1
do j0 = m, m+n-1
 do i0 = 0, m-1
  call mg22111_all (mxd, mm*nn)
 enddo
enddo
return
CONTAINS
SUBROUTINE mg22111_all (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,j0)*d(i0,k0)/mu
 pv(1) = pv(1)+d(i0,j0)*d(j0,k0)/mu
 pv(2) = pv(2)+d(i0,j0)*d(i0,l0)/mu
 pv(3) = pv(3)+d(i0,k0)*d(i0,l0)/mu
 pv(4) = pv(4)+d(i0,j0)*d(j0,l0)/mu
 pv(5) = pv(5)+d(j0,k0)*d(j0,l0)/mu
 pv(6) = pv(6)+d(i0,j0)*d(i0,m0)/mu
 pv(7) = pv(7)+d(i0,k0)*d(i0,m0)/mu
 pv(8) = pv(8)+d(i0,l0)*d(i0,m0)/mu
 pv(9) = pv(9)+d(i0,j0)*d(j0,m0)/mu
 pv(10) = pv(10)+d(j0,k0)*d(j0,m0)/mu
 pv(11) = pv(11)+d(j0,l0)*d(j0,m0)/mu
endif
if (3.le.mxd) then
 pv(12) = pv(12)+d3(i0,j0)/mu
 pv(13) = pv(13)+d2(i0,j0)*d(i0,k0)/mu
 pv(14) = pv(14)+d2(i0,j0)*d(j0,k0)/mu
 pv(15) = pv(15)+d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(16) = pv(16)+d2(i0,j0)*d(i0,l0)/mu
 pv(17) = pv(17)+d(i0,j0)*d(j0,k0)*d(i0,l0)/mu
 pv(18) = pv(18)+d2(i0,j0)*d(j0,l0)/mu
 pv(19) = pv(19)+d(i0,j0)*d(i0,k0)*d(j0,l0)/mu
 pv(20) = pv(20)+d(i0,j0)*d(i0,l0)*d(j0,l0)/mu
 pv(21) = pv(21)+d2(i0,j0)*d(i0,m0)/mu
 pv(22) = pv(22)+d(i0,j0)*d(j0,k0)*d(i0,m0)/mu
 pv(23) = pv(23)+d(i0,j0)*d(j0,l0)*d(i0,m0)/mu
 pv(24) = pv(24)+d2(i0,j0)*d(j0,m0)/mu
 pv(25) = pv(25)+d(i0,j0)*d(i0,k0)*d(j0,m0)/mu
 pv(26) = pv(26)+d(i0,j0)*d(i0,l0)*d(j0,m0)/mu
 pv(27) = pv(27)+d(i0,j0)*d(i0,m0)*d(j0,m0)/mu
endif
END SUBROUTINE mg22111_all
END SUBROUTINE mg22111_isecs
