SUBROUTINE sym4_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
if (size(x).ne.sym4_nr.or.size(u).ne.sym4_nr) then
 stop 'sym4_prims: bad dimensions'
endif
u(0) = sum(x)/size(x)
u(1) = sum(x**2)/size(x)
u(2) = sum(x**3)/size(x)
u(3) = sum(x**4)/size(x)
return
END SUBROUTINE sym4_prims
