SUBROUTINE mg82_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg82_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg82_prib: bad size u'
!! else if (size(w).ne.mg82_npb(mxd)) then
!!  stop 'mg82_prib: bad size w'
endif
END SUBROUTINE mg82_prib
