SUBROUTINE t3_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
if (size(x).ne.t3_nr.or.size(v).ne.t3_nsc(mxd)) then
 stop 't3_secs: bad dimensions'
endif
! There is only the trivial secondary
v(0) = 1
return
END SUBROUTINE t3_secs
