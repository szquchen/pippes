SUBROUTINE sym4_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
if (size(x).ne.sym4_nr.or.size(v).ne.sym4_nsc(mxd)) then
 stop 'sym4_secs: bad dimensions'
endif
! There is only the trivial secondary
v(0) = 1
return
END SUBROUTINE sym4_secs
