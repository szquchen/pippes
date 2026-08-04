SUBROUTINE ms32_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
integer, parameter :: npv=3, n=3
real (kind=wp) :: pv(0:npv-1)
if (size(x).ne.ms32_nr.or.size(v).ne.ms32_nsc(mxd)) then
 stop 'ms32_secs: bad dimensions'
endif
pv = 0
pv(0) = sum(x(0:n-1)*x(n:2*n-1))/n
pv(1) = sum(x(0:n-1)**2*x(n:2*n-1))/n
pv(2) = sum(x(0:n-1)*x(n:2*n-1)**2)/n
v(0) = 1
if (2.le.mxd) then
 v(1) = pv(0)
endif
if (3.le.mxd) then
 v(2:3) = pv(1:2)
endif
if (4.le.mxd) then
 v(4) = pv(0)*pv(0)
endif
if (6.le.mxd) then
 v(5) = pv(1)*pv(2)
endif
return
END SUBROUTINE ms32_secs
