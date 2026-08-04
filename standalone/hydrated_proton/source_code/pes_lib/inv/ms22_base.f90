SUBROUTINE ms22_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:ms22_nr-1), v(0:ms22_nsc(mxd)-1)
if (size(x).ne.ms22_nr.or.size(w).ne.ms22_nb(mxd)) then
 stop 'ms22_base: bad dimensions'
endif
call ms22_prims (x, u)
call ms22_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 2 2 0 0 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 1 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 2 6 10 19 28 44 60 85 110
! constant term
w(0) = v(0)
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(3:4) = u(0)*w(1:2)
 w(5) = u(1)*w(2)
 w(6) = u(2)*w(0)
 w(7) = u(3)*w(0)
 w(8) = v(1)
endif
! terms of degree 3
if (3.le.mxd) then
 w(9:14) = u(0)*w(3:8)
 w(15:18) = u(1)*w(5:8)
endif
! terms of degree 4
if (4.le.mxd) then
 w(19:28) = u(0)*w(9:18)
 w(29:32) = u(1)*w(15:18)
 w(33:35) = u(2)*w(6:8)
 w(36:37) = u(3)*w(7:8)
endif
! terms of degree 5
if (5.le.mxd) then
 w(38:56) = u(0)*w(19:37)
 w(57:65) = u(1)*w(29:37)
endif
! terms of degree 6
if (6.le.mxd) then
 w(66:93) = u(0)*w(38:65)
 w(94:102) = u(1)*w(57:65)
 w(103:107) = u(2)*w(33:37)
 w(108:109) = u(3)*w(36:37)
endif
! terms of degree 7
if (7.le.mxd) then
 w(110:153) = u(0)*w(66:109)
 w(154:169) = u(1)*w(94:109)
endif
! terms of degree 8
if (8.le.mxd) then
 w(170:229) = u(0)*w(110:169)
 w(230:245) = u(1)*w(154:169)
 w(246:252) = u(2)*w(103:109)
 w(253:254) = u(3)*w(108:109)
endif
! terms of degree 9
if (9.le.mxd) then
 w(255:339) = u(0)*w(170:254)
 w(340:364) = u(1)*w(230:254)
endif
END SUBROUTINE ms22_base
