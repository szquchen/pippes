SUBROUTINE mg51_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=5, m2=m*(m-1)/2
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg51_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg5_prims (x(0:m2-1), u0)
call sym5_prims (x(m2:m2+m-1), u1)
u = (/ u0(0), u1(0), &
  u0(1), u0(2), u1(1), &
  u0(3), u0(4), u1(2), &
  u0(5), u0(6), u1(3), &
  u0(7), u0(8), u1(4), &
  u0(9) /)
return
END SUBROUTINE mg51_prims
