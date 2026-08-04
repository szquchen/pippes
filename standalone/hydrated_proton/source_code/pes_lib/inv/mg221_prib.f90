SUBROUTINE mg221_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg221_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg221_prib: bad size u'
!! else if (size(w).ne.mg221_npb(mxd)) then
!!  stop 'mg221_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 5 5 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 5 20 60 160 376 820 1660 3190 5830
! constant term
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
 w(3) = u(2)*w(0)
 w(4) = u(3)*w(0)
 w(5) = u(4)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(6:10) = u(0)*w(1:5)
 w(11:14) = u(1)*w(2:5)
 w(15:17) = u(2)*w(3:5)
 w(18:19) = u(3)*w(4:5)
 w(20) = u(4)*w(5)
 w(21) = u(5)*w(0)
 w(22) = u(6)*w(0)
 w(23) = u(7)*w(0)
 w(24) = u(8)*w(0)
 w(25) = u(9)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(26:45) = u(0)*w(6:25)
 w(46:60) = u(1)*w(11:25)
 w(61:71) = u(2)*w(15:25)
 w(72:79) = u(3)*w(18:25)
 w(80:85) = u(4)*w(20:25)
endif
! terms of degree 4
if (4.le.mxd) then
 w(86:145) = u(0)*w(26:85)
 w(146:185) = u(1)*w(46:85)
 w(186:210) = u(2)*w(61:85)
 w(211:224) = u(3)*w(72:85)
 w(225:230) = u(4)*w(80:85)
 w(231:235) = u(5)*w(21:25)
 w(236:239) = u(6)*w(22:25)
 w(240:242) = u(7)*w(23:25)
 w(243:244) = u(8)*w(24:25)
 w(245) = u(9)*w(25)
endif
! terms of degree 5
if (5.le.mxd) then
 w(246:405) = u(0)*w(86:245)
 w(406:505) = u(1)*w(146:245)
 w(506:565) = u(2)*w(186:245)
 w(566:600) = u(3)*w(211:245)
 w(601:621) = u(4)*w(225:245)
endif
! terms of degree 6
if (6.le.mxd) then
 w(622:997) = u(0)*w(246:621)
 w(998:1213) = u(1)*w(406:621)
 w(1214:1329) = u(2)*w(506:621)
 w(1330:1385) = u(3)*w(566:621)
 w(1386:1406) = u(4)*w(601:621)
 w(1407:1421) = u(5)*w(231:245)
 w(1422:1431) = u(6)*w(236:245)
 w(1432:1437) = u(7)*w(240:245)
 w(1438:1440) = u(8)*w(243:245)
 w(1441) = u(9)*w(245)
endif
! terms of degree 7
if (7.le.mxd) then
 w(1442:2261) = u(0)*w(622:1441)
 w(2262:2705) = u(1)*w(998:1441)
 w(2706:2933) = u(2)*w(1214:1441)
 w(2934:3045) = u(3)*w(1330:1441)
 w(3046:3101) = u(4)*w(1386:1441)
endif
! terms of degree 8
if (8.le.mxd) then
 w(3102:4761) = u(0)*w(1442:3101)
 w(4762:5601) = u(1)*w(2262:3101)
 w(5602:5997) = u(2)*w(2706:3101)
 w(5998:6165) = u(3)*w(2934:3101)
 w(6166:6221) = u(4)*w(3046:3101)
 w(6222:6256) = u(5)*w(1407:1441)
 w(6257:6276) = u(6)*w(1422:1441)
 w(6277:6286) = u(7)*w(1432:1441)
 w(6287:6290) = u(8)*w(1438:1441)
 w(6291) = u(9)*w(1441)
endif
! terms of degree 9
if (9.le.mxd) then
 w(6292:9481) = u(0)*w(3102:6291)
 w(9482:11011) = u(1)*w(4762:6291)
 w(11012:11701) = u(2)*w(5602:6291)
 w(11702:11995) = u(3)*w(5998:6291)
 w(11996:12121) = u(4)*w(6166:6291)
endif
END SUBROUTINE mg221_prib
