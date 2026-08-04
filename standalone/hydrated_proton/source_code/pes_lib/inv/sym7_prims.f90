SUBROUTINE sym7_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
if (size(x).ne.sym7_nr.or.size(u).ne.sym7_nr) then
 stop 'sym7_prims: bad dimensions'
endif
u(0) = sum(x)/size(x)
u(1) = sum(x**2)/size(x)
u(2) = sum(x**3)/size(x)
u(3) = sum(x**4)/size(x)
u(4) = sum(x**5)/size(x)
u(5) = sum(x**6)/size(x)
u(6) = sum(x**7)/size(x)
return
END SUBROUTINE sym7_prims
