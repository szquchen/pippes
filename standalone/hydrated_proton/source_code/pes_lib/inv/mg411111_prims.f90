SUBROUTINE mg411111_prims (r, u)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=4, m2=m*(m-1)/2
real (kind=wp) :: x(0:nr-1), u0(0:m2-1), u1(0:m-1), u2(0:m-1), u3(0:0), &
  u4(0:m-1), u5(0:0), u6(0:0), u7(0:m-1), u8(0:0), u9(0:0), u10(0:0), &
  u11(0:m-1), u12(0:0), u13(0:0), u14(0:0), u15(0:0)
if (size(r,1).ne.nk.or.size(r,2).ne.nk.or.size(u).ne.nr) then
 stop 'mg411111_prims: bad dimensions'
endif
call mgx_mk1d (nkj, r, x)
call cg4_prims (x(0:m2-1), u0)
call sym4_prims (x(m2:m2+m-1), u1)
call sym4_prims (x(m2+m:m2+2*m-1), u2)
u3(0) = x(m2+2*m)
call sym4_prims (x(m2+2*m+1:m2+3*m), u4)
u5(0) = x(m2+3*m+1)
u6(0) = x(m2+3*m+2)
call sym4_prims (x(m2+3*m+3:m2+4*m+2), u7)
u8(0) = x(m2+4*m+3)
u9(0) = x(m2+4*m+4)
u10(0) = x(m2+4*m+5)
call sym4_prims (x(m2+4*m+6:m2+5*m+5), u11)
u12(0) = x(m2+5*m+6)
u13(0) = x(m2+5*m+7)
u14(0) = x(m2+5*m+8)
u15(0) = x(m2+5*m+9)
u = (/ u0(0), u1(0), u2(0), u3(0), u4(0), u5(0), u6(0), &
  u7(0), u8(0), u9(0), u10(0), &
  u11(0), u12(0), u13(0), u14(0), u15(0), &
  u0(1), u0(2), u1(1), u2(1), u4(1), u7(1), u11(1), &
  u0(3), u0(4), u1(2), u2(2), u4(2), u7(2), u11(2), &
  u0(5), u1(3), u2(3), u4(3), u7(3), u11(3) /)
return
END SUBROUTINE mg411111_prims
