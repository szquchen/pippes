SUBROUTINE mg621_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=6, n=2, m2=m*(m-1)/2, n2=n*(n-1)/2
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m*n-1), u2(0:n2-1), &
  u3(0:m-1), u4(0:n-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg621_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg6_prims (x(0:m2-1), u0)
call bg62_prims (x(m2:m2+m*n-1), u1)
call cg2_prims (x(m2+m*n:m2+m*n+n2-1), u2)
call sym6_prims (x(m2+m*n+n2:m2+m*n+n2+m-1), u3)
call sym2_prims (x(m2+m*n+n2+m:nr-1), u4)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), &
  u0(1), u0(2), u1(1), u1(2), u1(3), u3(1), u4(1), &
  u0(3), u0(4), u0(5), u0(6), u1(4), u1(5), u3(2), &
  u0(7), u0(8), u0(9), u1(6), u1(7), u3(3), &
  u0(10), u0(11), u0(12), u1(8), u3(4), &
  u0(13), u0(14), u1(9), u1(10), u3(5), &
  u1(11) /)
return
END SUBROUTINE mg621_prims
