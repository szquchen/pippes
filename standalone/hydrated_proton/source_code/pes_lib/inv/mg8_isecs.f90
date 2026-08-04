SUBROUTINE mg8_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=8
integer :: i0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5, d6
if (size(r).ne.nk*nk) then
 stop 'mg8_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5, d6)
pv = 0
return
END SUBROUTINE mg8_isecs
