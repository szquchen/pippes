SUBROUTINE mg221_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, n=2, mm=m, nn=n
integer :: i0, j0, k0, mu
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3
if (size(r).ne.nk*nk) then
 stop 'mg221_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3)
pv = 0
k0 = m+n
do j0 = m, m+n-1
 do i0 = 0, m-1
  call mg221_all (mxd, mm*nn)
 enddo
enddo
return
CONTAINS
SUBROUTINE mg221_all (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,j0)*d(i0,k0)/mu
 pv(1) = pv(1)+d(i0,j0)*d(j0,k0)/mu
endif
if (3.le.mxd) then
 pv(2) = pv(2)+d3(i0,j0)/mu
 pv(3) = pv(3)+d2(i0,j0)*d(i0,k0)/mu
 pv(4) = pv(4)+d2(i0,j0)*d(j0,k0)/mu
 pv(5) = pv(5)+d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
endif
END SUBROUTINE mg221_all
END SUBROUTINE mg221_isecs
