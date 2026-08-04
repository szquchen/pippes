SUBROUTINE mg82_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=8, n=2, m2=m*(m-1)/2, n2=n*(n-1)/2
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m*n-1), u2(0:n2-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg82_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg8_prims (x(0:m2-1), u0)
call bg82_prims (x(m2:m2+m*n-1), u1)
call cg2_prims (x(m2+m*n:m2+m*n+n2-1), u2)
u = (/ ... /)
return
END SUBROUTINE mg82_prims
