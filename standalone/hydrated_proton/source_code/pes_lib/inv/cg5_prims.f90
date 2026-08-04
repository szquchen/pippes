SUBROUTINE cg5_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: t1(0:nk-1)
if (size(x).ne.nr.or.size(u).ne.nr) then
 stop 'cg5_prims: bad dimensions'
endif
! Recall revlex order,
! x(0:9) = (/ d01, d02, d12, d03, d13, d23, d04, d14, d24, d34 /)
t1(0) = (x(0)+x(1)+x(3)+x(6))/4
t1(1) = (x(0)+x(2)+x(4)+x(7))/4
t1(2) = (x(1)+x(2)+x(5)+x(8))/4
t1(3) = (x(3)+x(4)+x(5)+x(9))/4
t1(4) = (x(6)+x(7)+x(8)+x(9))/4
u(0) = sum(x)/size(x)
u(1) = sum(t1**2)/size(t1)
u(2) = sum(x**2)/size(x)
u(3) = sum(t1**3)/size(t1)
u(4) = sum(x**3)/size(x)
u(5) = sum(t1**4)/size(t1)
u(6) = sum(x**4)/size(x)
u(7) = sum(t1**5)/size(t1)
u(8) = sum(x**5)/size(x)
u(9) = sum(x**6)/size(x)
return
END SUBROUTINE cg5_prims
