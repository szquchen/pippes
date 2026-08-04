SUBROUTINE ms23_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:ms23_nr-1), v(0:ms23_nsc(mxd)-1)
if (size(x).ne.ms23_nr.or.size(w).ne.ms23_nb(mxd)) then
 stop 'ms23_base: bad dimensions'
endif
call ms23_prims (x, u)
call ms23_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 3 3 0 0 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 3 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 3 12 28 66 126 236 396 651 1001
! constant term
w(0) = v(0)
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
 w(3) = u(2)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(4:6) = u(0)*w(1:3)
 w(7:8) = u(1)*w(2:3)
 w(9) = u(2)*w(3)
 w(10) = u(3)*w(0)
 w(11) = u(4)*w(0)
 w(12) = u(5)*w(0)
 w(13:15) = v(1:3)
endif
! terms of degree 3
if (3.le.mxd) then
 w(16:27) = u(0)*w(4:15)
 w(28:36) = u(1)*w(7:15)
 w(37:43) = u(2)*w(9:15)
endif
! terms of degree 4
if (4.le.mxd) then
 w(44:71) = u(0)*w(16:43)
 w(72:87) = u(1)*w(28:43)
 w(88:94) = u(2)*w(37:43)
 w(95:100) = u(3)*w(10:15)
 w(101:105) = u(4)*w(11:15)
 w(106:109) = u(5)*w(12:15)
endif
! terms of degree 5
if (5.le.mxd) then
 w(110:175) = u(0)*w(44:109)
 w(176:213) = u(1)*w(72:109)
 w(214:235) = u(2)*w(88:109)
endif
! terms of degree 6
if (6.le.mxd) then
 w(236:361) = u(0)*w(110:235)
 w(362:421) = u(1)*w(176:235)
 w(422:443) = u(2)*w(214:235)
 w(444:458) = u(3)*w(95:109)
 w(459:467) = u(4)*w(101:109)
 w(468:471) = u(5)*w(106:109)
endif
! terms of degree 7
if (7.le.mxd) then
 w(472:707) = u(0)*w(236:471)
 w(708:817) = u(1)*w(362:471)
 w(818:867) = u(2)*w(422:471)
endif
! terms of degree 8
if (8.le.mxd) then
 w(868:1263) = u(0)*w(472:867)
 w(1264:1423) = u(1)*w(708:867)
 w(1424:1473) = u(2)*w(818:867)
 w(1474:1501) = u(3)*w(444:471)
 w(1502:1514) = u(4)*w(459:471)
 w(1515:1518) = u(5)*w(468:471)
endif
! terms of degree 9
if (9.le.mxd) then
 w(1519:2169) = u(0)*w(868:1518)
 w(2170:2424) = u(1)*w(1264:1518)
 w(2425:2519) = u(2)*w(1424:1518)
endif
END SUBROUTINE ms23_base
