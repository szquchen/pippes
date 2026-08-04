SUBROUTINE cg4_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: t1(0:nk-1)
if (size(x).ne.nr.or.size(u).ne.nr) then
 stop 'cg4_prims: bad dimensions'
endif
! Recall revlex order,
! x(0:5) = (/ d01, d02, d12, d03, d13, d23 /)
t1(0) = (x(0)+x(1)+x(3))/3
t1(1) = (x(0)+x(2)+x(4))/3
t1(2) = (x(1)+x(2)+x(5))/3
t1(3) = (x(3)+x(4)+x(5))/3
u(0) = sum(x)/size(x)
u(1) = sum(t1**2)/size(t1)
u(2) = sum(x**2)/size(x)
u(3) = sum(t1**3)/size(t1)
u(4) = sum(x**3)/size(x)
u(5) = sum(t1**4)/size(t1)
return
END SUBROUTINE cg4_prims
