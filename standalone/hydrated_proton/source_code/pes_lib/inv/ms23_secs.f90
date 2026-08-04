SUBROUTINE ms23_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
integer, parameter :: npv=3, n=2
real (kind=wp) :: pv(0:npv-1)
if (size(x).ne.ms23_nr.or.size(v).ne.ms23_nsc(mxd)) then
 stop 'ms23_secs: bad dimensions'
endif
pv = 0
pv(0) = sum(x(0:n-1)*x(n:2*n-1))/n
pv(1) = sum(x(0:n-1)*x(2*n:3*n-1))/n
pv(2) = sum(x(n:2*n-1)*x(2*n:3*n-1))/n
v(0) = 1
if (2.le.mxd) then
 v(1:3) = pv(0:2)
endif
return
END SUBROUTINE ms23_secs
