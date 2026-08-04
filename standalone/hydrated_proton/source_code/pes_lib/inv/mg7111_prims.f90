SUBROUTINE mg7111_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=7, m2=m*(m-1)/2
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m-1), u2(0:m-1), u3(0:0), &
  u4(0:m-1), u5(0:0), u6(0:0)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg7111_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg7_prims (x(0:m2-1), u0)
call sym7_prims (x(m2:m2+m-1), u1)
call sym7_prims (x(m2+m:m2+2*m-1), u2)
u3(0) = x(m2+2*m)
call sym7_prims (x(m2+2*m+1:m2+3*m), u4)
u5(0) = x(m2+3*m+1)
u6(0:0) = x(m2+3*m+2:nr-1)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), u6(0), &
  u0(1), u0(2), u1(1), u2(1), u4(1), &
  u0(3), u0(4), u0(5), u0(6), u0(7), u1(2), u2(2), u4(2), &
  u0(8), u0(9), u0(10), u1(3), u2(3), u4(3), &
  u0(11), u0(12), u0(13), u1(4), u2(4), u4(4), &
  u0(14), u0(15), u1(5), u2(5), u4(5), &
  u0(16), u0(17), u0(18), u1(6), u2(6), u4(6), &
  u0(19), u0(20) /)
return
END SUBROUTINE mg7111_prims
