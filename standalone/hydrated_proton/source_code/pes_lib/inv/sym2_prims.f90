SUBROUTINE sym2_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
if (size(x).ne.sym2_nr.or.size(u).ne.sym2_nr) then
 stop 'sym2_prims: bad dimensions'
endif
u(0) = sum(x)/size(x)
u(1) = sum(x**2)/size(x)
return
END SUBROUTINE sym2_prims
