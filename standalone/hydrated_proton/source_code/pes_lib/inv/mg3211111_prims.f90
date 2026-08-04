SUBROUTINE mg3211111_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=3, n=2, m2=m*(m-1)/2, n2=n*(n-1)/2
integer :: ib0
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m*n-1), u2(0:n2-1), &
  u3(0:m-1), u4(0:n-1), u5(0:m-1), u6(0:n-1), u7(0:0), &
  u8(0:m-1), u9(0:n-1), u10(0:0), u11(0:0), &
  u12(0:m-1), u13(0:n-1), u14(0:0), u15(0:0), u16(0:0), &
  u17(0:m-1), u18(0:n-1), u19(0:0), u20(0:0), u21(0:0), u22(0:0)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg3211111_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg3_prims (x(0:m2-1), u0)
call bg32_prims (x(m2:m2+m*n-1), u1)
call cg2_prims (x(m2+m*n:m2+m*n+n2-1), u2)
call sym3_prims (x(m2+m*n+n2:m2+m*n+n2+m-1), u3)
call sym2_prims (x(m2+m*n+n2+m:m2+m*n+n2+m+n-1), u4)
call sym3_prims (x(m2+m*n+n2+m+n:m2+m*n+n2+2*m+n-1), u5)
call sym2_prims (x(m2+m*n+n2+2*m+n:m2+m*n+n2+2*m+2*n-1), u6)
u7(0) = x(m2+m*n+n2+2*m+2*n)
ib0 = m2+m*n+n2+2*m+2*n+1
call sym3_prims (x(ib0:ib0+m-1), u8)
call sym2_prims (x(ib0+m:ib0+m+n-1), u9)
u10(0) = x(ib0+m+n)
u11(0) = x(ib0+m+n+1)
ib0 = ib0+m+n+2
call sym3_prims (x(ib0:ib0+m-1), u12)
call sym2_prims (x(ib0+m:ib0+m+n-1), u13)
u14(0) = x(ib0+m+n)
u15(0) = x(ib0+m+n+1)
u16(0) = x(ib0+m+n+2)
ib0 = ib0+m+n+3
call sym3_prims (x(ib0:ib0+m-1), u17)
call sym2_prims (x(ib0+m:ib0+m+n-1), u18)
u19(0) = x(ib0+m+n)
u20(0) = x(ib0+m+n+1)
u21(0) = x(ib0+m+n+2)
u22(0) = x(ib0+m+n+3)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), u6(0), u7(0), &
  u8(0), u9(0), u10(0), u11(0), &
  u12(0), u13(0), u14(0), u15(0), u16(0), &
  u17(0), u18(0), u19(0), u20(0), u21(0), u22(0), &
  u0(1), u1(1), u1(2), u1(3), u3(1), u4(1), u5(1), u6(1), u8(1), u9(1), &
  u12(1), u13(1), u17(1), u18(1), &
  u0(2), u1(4), u3(2), u5(2), u8(2), u12(2), u17(2), &
  u1(5) /)
return
END SUBROUTINE mg3211111_prims
