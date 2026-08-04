SUBROUTINE mg21_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg21_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg21_prib: bad size u'
!! else if (size(w).ne.mg21_npb(mxd)) then
!!  stop 'mg21_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 2 1 0 0 0 0 0 0 0 &
!   0 0 0 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 2 4 6 9 12 16 20 25 30 &
!   36 42 49 56 64 72 81 90 100 110
! constant term
w(0) = 1
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
endif
! terms of degree 3
if (3.le.mxd) then
 w(7:10) = u(0)*w(3:6)
 w(11:12) = u(1)*w(5:6)
endif
! terms of degree 4
if (4.le.mxd) then
 w(13:18) = u(0)*w(7:12)
 w(19:20) = u(1)*w(11:12)
 w(21) = u(2)*w(6)
endif
! terms of degree 5
if (5.le.mxd) then
 w(22:30) = u(0)*w(13:21)
 w(31:33) = u(1)*w(19:21)
endif
! terms of degree 6
if (6.le.mxd) then
 w(34:45) = u(0)*w(22:33)
 w(46:48) = u(1)*w(31:33)
 w(49) = u(2)*w(21)
endif
! terms of degree 7
if (7.le.mxd) then
 w(50:65) = u(0)*w(34:49)
 w(66:69) = u(1)*w(46:49)
endif
! terms of degree 8
if (8.le.mxd) then
 w(70:89) = u(0)*w(50:69)
 w(90:93) = u(1)*w(66:69)
 w(94) = u(2)*w(49)
endif
! terms of degree 9
if (9.le.mxd) then
 w(95:119) = u(0)*w(70:94)
 w(120:124) = u(1)*w(90:94)
endif
! terms of degree 10
if (10.le.mxd) then
 w(125:154) = u(0)*w(95:124)
 w(155:159) = u(1)*w(120:124)
 w(160) = u(2)*w(94)
endif
! terms of degree 11
if (11.le.mxd) then
 w(161:196) = u(0)*w(125:160)
 w(197:202) = u(1)*w(155:160)
endif
! terms of degree 12
if (12.le.mxd) then
 w(203:244) = u(0)*w(161:202)
 w(245:250) = u(1)*w(197:202)
 w(251) = u(2)*w(160)
endif
! terms of degree 13
if (13.le.mxd) then
 w(252:300) = u(0)*w(203:251)
 w(301:307) = u(1)*w(245:251)
endif
! terms of degree 14
if (14.le.mxd) then
 w(308:363) = u(0)*w(252:307)
 w(364:370) = u(1)*w(301:307)
 w(371) = u(2)*w(251)
endif
! terms of degree 15
if (15.le.mxd) then
 w(372:435) = u(0)*w(308:371)
 w(436:443) = u(1)*w(364:371)
endif
! terms of degree 16
if (16.le.mxd) then
 w(444:515) = u(0)*w(372:443)
 w(516:523) = u(1)*w(436:443)
 w(524) = u(2)*w(371)
endif
! terms of degree 17
if (17.le.mxd) then
 w(525:605) = u(0)*w(444:524)
 w(606:614) = u(1)*w(516:524)
endif
! terms of degree 18
if (18.le.mxd) then
 w(615:704) = u(0)*w(525:614)
 w(705:713) = u(1)*w(606:614)
 w(714) = u(2)*w(524)
endif
! terms of degree 19
if (19.le.mxd) then
 w(715:814) = u(0)*w(615:714)
 w(815:824) = u(1)*w(705:714)
endif
END SUBROUTINE mg21_prib
