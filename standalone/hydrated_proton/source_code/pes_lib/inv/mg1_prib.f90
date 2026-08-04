SUBROUTINE mg1_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg1_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg1_prib: bad size u'
!! else if (size(w).ne.mg1_npb(mxd)) then
!!  stop 'mg1_prib: bad size w'
endif
! constant term
w(0) = 1
END SUBROUTINE mg1_prib
