SUBROUTINE cg2_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
!! real (kind=wp), intent (out) :: v(0:)
real (kind=wp), intent (out) :: v(0:0)
!-----------------------------------------------------------------------
if (size(x).ne.nr.or.size(v).ne.cg2_nsc(mxd)) then
 stop 'cg2_secs: bad dimensions'
endif
! There is only the trivial secondary
v(0) = 1
return
END SUBROUTINE cg2_secs
