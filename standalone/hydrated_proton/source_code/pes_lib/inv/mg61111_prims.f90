SUBROUTINE mg61111_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=6, m2=m*(m-1)/2
integer :: ib0
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m-1), u2(0:m-1), u3(0:0), &
  u4(0:m-1), u5(0:0), u6(0:0), &
  u7(0:m-1), u8(0:0), u9(0:0), u10(0:0)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg61111_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg6_prims (x(0:m2-1), u0)
call sym6_prims (x(m2:m2+m-1), u1)
call sym6_prims (x(m2+m:m2+2*m-1), u2)
u3(0) = x(m2+2*m)
ib0 = m2+2*m+1
call sym6_prims (x(ib0:ib0+m-1), u4)
u5(0) = x(ib0+m)
u6(0) = x(ib0+m+1)
ib0 = ib0+m+2
call sym6_prims (x(ib0:ib0+m-1), u7)
u8(0) = x(ib0+m)
u9(0) = x(ib0+m+1)
u10(0:0) = x(ib0+m+2:nr-1)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), u6(0), &
  u7(0), u8(0), u9(0), u10(0), &
  u0(1), u0(2), u1(1), u2(1), u4(1), u7(1), &
  u0(3), u0(4), u0(5), u0(6), u1(2), u2(2), u4(2), u7(2), &
  u0(7), u0(8), u0(9), u1(3), u2(3), u4(3), u7(3), &
  u0(10), u0(11), u0(12), u1(4), u2(4), u4(4), u7(4), &
  u0(13), u0(14), u1(5), u2(5), u4(5), u7(5) /)
return
END SUBROUTINE mg61111_prims
