SUBROUTINE mg3_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
if (size(r).ne.nk*nk.or.size(v).ne.mg3_nsc(mxd)) then
 stop 'mg3_secs: bad dimensions'
endif
! There is only the trivial secondary
v(0) = 1
return
END SUBROUTINE mg3_secs
