SUBROUTINE sym2s2_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
if (size(x).ne.sym2s2_nr.or.size(u).ne.sym2s2_nr) then
 stop 'sym2s2_prims: bad dimensions'
endif
u(0) = sum(x(0:1))/2
u(1) = sum(x(2:3))/2
u(2) = sum(x(0:1)**2)/2
u(3) = sum(x(2:3)**2)/2
return
END SUBROUTINE sym2s2_prims
