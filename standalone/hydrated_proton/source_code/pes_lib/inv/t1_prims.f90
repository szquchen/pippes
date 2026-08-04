SUBROUTINE t1_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
if (size(x).ne.t1_nr.or.size(u).ne.t1_nr) then
 stop 't1_prims: bad dimensions'
endif
! There is just one variable
u = x
return
END SUBROUTINE t1_prims
