SUBROUTINE sym2t2_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
if (size(x).ne.sym2t2_nr.or.size(u).ne.sym2t2_nr) then
 stop 'sym2t2_prims: bad dimensions'
endif
u(0) = sum(x(0:1))/2
u(1) = x(2)
u(2) = x(3)
u(3) = sum(x(0:1)**2)/2
return
END SUBROUTINE sym2t2_prims
