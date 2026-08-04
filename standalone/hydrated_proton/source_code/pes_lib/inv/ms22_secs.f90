SUBROUTINE ms22_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
integer, parameter :: npv=1, n=2
real (kind=wp) :: pv(0:npv-1)
if (size(x).ne.ms22_nr.or.size(v).ne.ms22_nsc(mxd)) then
 stop 'ms22_secs: bad dimensions'
endif
pv = 0
pv(0) = sum(x(0:n-1)*x(n:2*n-1))/n
v(0) = 1
if (2.le.mxd) then
 v(1) = pv(0)
endif
return
END SUBROUTINE ms22_secs
