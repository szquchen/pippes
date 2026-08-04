SUBROUTINE bg22_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
integer, parameter :: npv=1
real (kind=wp) :: pv(0:npv-1)
if (size(x).ne.bg22_nr.or.size(v).ne.bg22_nsc(mxd)) then
 stop 'bg22_secs: bad dimensions'
endif
pv = 0
pv(0) = sum(x**3)/size(x)
v(0) = 1
if (3.le.mxd) then
 v(1) = pv(0)
endif
return
END SUBROUTINE bg22_secs
