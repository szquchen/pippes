SUBROUTINE mg11_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg11_prims: bad dimensions'
endif
! There is just one variable
u(0) = (r(0,1)+r(1,0))/2
return
END SUBROUTINE mg11_prims
