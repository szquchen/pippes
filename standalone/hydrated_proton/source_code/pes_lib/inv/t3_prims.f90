SUBROUTINE t3_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
if (size(x).ne.t3_nr.or.size(u).ne.t3_nr) then
 stop 't3_prims: bad dimensions'
endif
! Note, each variable is a primary generator
u = x
return
END SUBROUTINE t3_prims
