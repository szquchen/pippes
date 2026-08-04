SUBROUTINE mg22111_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, n=2, m2=m*(m-1)/2, n2=n*(n-1)/2
integer :: ib0
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m*n-1), u2(0:n2-1), u3(0:m-1), &
  u4(0:n-1), u5(0:m-1), u6(0:n-1), u7(0:0), u8(0:m-1), u9(0:n-1), &
  u10(0:0), u11(0:0)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg22111_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg2_prims (x(0:m2-1), u0)
call bg22_prims (x(m2:m2+m*n-1), u1)
call cg2_prims (x(m2+m*n:m2+m*n+n2-1), u2)
call sym2_prims (x(m2+m*n+n2:m2+m*n+n2+m-1), u3)
call sym2_prims (x(m2+m*n+n2+m:m2+m*n+n2+m+n-1), u4)
ib0 = m2+m*n+n2+m+n
call sym2_prims (x(ib0:ib0+m-1), u5)
call sym2_prims (x(ib0+m:ib0+m+n-1), u6)
u7(0) = x(ib0+m+n)
ib0 = ib0+m+n+1
call sym2_prims (x(ib0:ib0+m-1), u8)
call sym2_prims (x(ib0+m:ib0+m+n-1), u9)
u10(0) = x(ib0+m+n)
u11(0) = x(ib0+m+n+1)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), u6(0), u7(0), &
  u8(0), u9(0), u10(0), u11(0), &
  u1(1), u1(2), u1(3), u3(1), u4(1), u5(1), u6(1), u8(1), u9(1) /)
return
END SUBROUTINE mg22111_prims
