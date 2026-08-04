SUBROUTINE mg2222_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, n=2, l=2, p=2, m2=m*(m-1)/2, n2=n*(n-1)/2, &
  l2=l*(l-1)/2, p2=p*(p-1)/2
integer :: ib0
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m*n-1), u2(0:n2-1), &
  u3(0:m*l-1), u4(0:n*l-1), u5(0:l2-1), u6(0:m*p-1), &
  u7(0:n*p-1), u8(0:l*p-1), u9(0:p2-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg2222_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg2_prims (x(0:m2-1), u0)
call bg22_prims (x(m2:m2+m*n-1), u1)
call cg2_prims (x(m2+m*n:m2+m*n+n2-1), u2)
call bg22_prims (x(m2+m*n+n2:m2+m*n+n2+m*l-1), u3)
call bg22_prims (x(m2+m*n+n2+m*l:m2+m*n+n2+m*l+n*l-1), u4)
call cg2_prims (x(m2+m*n+n2+m*l+n*l:m2+m*n+n2+m*l+n*l+l2-1), u5)
ib0 = m2+m*n+n2+m*l+n*l+l2
call bg22_prims (x(ib0:ib0+m*p-1), u6)
call bg22_prims (x(ib0+m*p:ib0+m*p+n*p-1), u7)
call bg22_prims (x(ib0+m*p+n*p:ib0+(m+n+l)*p-1), u8)
call cg2_prims (x(ib0+(m+n+l)*p:nr-1), u9)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), u6(0), &
  u7(0), u8(0), u9(0), &
  u1(1), u1(2), u1(3), u3(1), u3(2), u3(3), u4(1), u4(2), u4(3), &
  u6(1), u6(2), u6(3), u7(1), u7(2), u7(3), u8(1), u8(2), u8(3) /)
return
END SUBROUTINE mg2222_prims
