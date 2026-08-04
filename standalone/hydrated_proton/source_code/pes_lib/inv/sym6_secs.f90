SUBROUTINE sym6_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
if (size(x).ne.sym6_nr.or.size(v).ne.sym6_nsc(mxd)) then
 stop 'sym6_secs: bad dimensions'
endif
! There is only the trivial secondary
v(0) = 1
return
END SUBROUTINE sym6_secs
