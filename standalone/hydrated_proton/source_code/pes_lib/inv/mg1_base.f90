SUBROUTINE mg1_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
!! real (kind=wp), intent (out) :: w(0:mg1_nb(mxd)-1)
real (kind=wp), intent (out) :: w(0:0)
!-----------------------------------------------------------------------
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mg1_base: bad size r'
!! else if (size(w).ne.mg1_nb(mxd)) then
!!  stop 'mg1_base: bad size w'
endif
! trivial code for this special case
call mg1_secs (mxd, r, w)
return
END SUBROUTINE mg1_base
