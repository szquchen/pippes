SUBROUTINE mg22_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, n=2, mm=m, nn=n
integer :: i0, j0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3
if (size(r).ne.nk*nk) then
 stop 'mg22_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3)
pv = 0
do j0 = m, m+n-1
 do i0 = 0, m-1
  call mg22_all (mxd, mm*nn)
 enddo
enddo
return
CONTAINS
SUBROUTINE mg22_all (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(0) = pv(0)+d3(i0,j0)/mu
endif
END SUBROUTINE mg22_all
END SUBROUTINE mg22_isecs
