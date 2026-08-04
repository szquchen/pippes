SUBROUTINE sym7_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:sym7_nr-1), v(0:sym7_nsc(mxd)-1)
if (size(x).ne.sym7_nr.or.size(w).ne.sym7_nb(mxd)) then
 stop 'sym7_base: bad dimensions'
endif
call sym7_prims (x, u)
call sym7_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 1 1 1 1 1 1 0 0
! secs,  dnsc(0:*): 1 0 0 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 1 2 3 5 7 11 15 21 28
! constant term
w(0) = v(0)
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(2) = u(0)*w(1)
 w(3) = u(1)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(4:5) = u(0)*w(2:3)
 w(6) = u(2)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(7:9) = u(0)*w(4:6)
 w(10) = u(1)*w(3)
 w(11) = u(3)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(12:16) = u(0)*w(7:11)
 w(17) = u(1)*w(6)
 w(18) = u(4)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(19:25) = u(0)*w(12:18)
 w(26:27) = u(1)*w(10:11)
 w(28) = u(2)*w(6)
 w(29) = u(5)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(30:40) = u(0)*w(19:29)
 w(41:42) = u(1)*w(17:18)
 w(43) = u(2)*w(11)
 w(44) = u(6)*w(0)
endif
! terms of degree 8
if (8.le.mxd) then
 w(45:59) = u(0)*w(30:44)
 w(60:63) = u(1)*w(26:29)
 w(64) = u(2)*w(18)
 w(65) = u(3)*w(11)
endif
! terms of degree 9
if (9.le.mxd) then
 w(66:86) = u(0)*w(45:65)
 w(87:90) = u(1)*w(41:44)
 w(91:92) = u(2)*w(28:29)
 w(93) = u(3)*w(18)
endif
END SUBROUTINE sym7_base
