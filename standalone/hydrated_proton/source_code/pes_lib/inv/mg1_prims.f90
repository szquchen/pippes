SUBROUTINE mg1_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer :: i
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg1_prims: bad dimensions'
endif
! There are no primaries
u = (/ (0.0_wp,i=1,0) /) ! an empty array of well-defined type
return
END SUBROUTINE mg1_prims
