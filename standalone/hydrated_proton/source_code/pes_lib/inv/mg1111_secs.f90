SUBROUTINE mg1111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
if (size(r).ne.nk*nk.or.size(v).ne.mg1111_nsc(mxd)) then
 stop 'mg1111_secs: bad dimensions'
endif
! There is only the trivial secondary
v(0) = 1
return
END SUBROUTINE mg1111_secs
