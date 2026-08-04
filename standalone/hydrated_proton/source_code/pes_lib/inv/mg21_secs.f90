SUBROUTINE mg21_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
if (size(r).ne.nk*nk.or.size(v).ne.mg21_nsc(mxd)) then
 stop 'mg21_secs: bad dimensions'
endif
v(0) = 1
! There are no further secondaries
return
END SUBROUTINE mg21_secs
