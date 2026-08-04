SUBROUTINE mg41_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg41_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg41_prib: bad size u'
!! else if (size(w).ne.mg41_npb(mxd)) then
!!  stop 'mg41_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 2 3 3 2 0 0 0 0 0
! prib,  dnpb(0:*): 1 2 6 13 28 52 98 168 286 462
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
 w(7) = u(3)*w(0)
 w(8) = u(4)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(9:14) = u(0)*w(3:8)
 w(15:18) = u(1)*w(5:8)
 w(19) = u(5)*w(0)
 w(20) = u(6)*w(0)
 w(21) = u(7)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(22:34) = u(0)*w(9:21)
 w(35:41) = u(1)*w(15:21)
 w(42:44) = u(2)*w(6:8)
 w(45:46) = u(3)*w(7:8)
 w(47) = u(4)*w(8)
 w(48) = u(8)*w(0)
 w(49) = u(9)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(50:77) = u(0)*w(22:49)
 w(78:92) = u(1)*w(35:49)
 w(93:95) = u(2)*w(19:21)
 w(96:98) = u(3)*w(19:21)
 w(99:101) = u(4)*w(19:21)
endif
! terms of degree 6
if (6.le.mxd) then
 w(102:153) = u(0)*w(50:101)
 w(154:177) = u(1)*w(78:101)
 w(178:185) = u(2)*w(42:49)
 w(186:190) = u(3)*w(45:49)
 w(191:193) = u(4)*w(47:49)
 w(194:196) = u(5)*w(19:21)
 w(197:198) = u(6)*w(20:21)
 w(199) = u(7)*w(21)
endif
! terms of degree 7
if (7.le.mxd) then
 w(200:297) = u(0)*w(102:199)
 w(298:343) = u(1)*w(154:199)
 w(344:352) = u(2)*w(93:101)
 w(353:358) = u(3)*w(96:101)
 w(359:361) = u(4)*w(99:101)
 w(362:363) = u(5)*w(48:49)
 w(364:365) = u(6)*w(48:49)
 w(366:367) = u(7)*w(48:49)
endif
! terms of degree 8
if (8.le.mxd) then
 w(368:535) = u(0)*w(200:367)
 w(536:605) = u(1)*w(298:367)
 w(606:627) = u(2)*w(178:199)
 w(628:641) = u(3)*w(186:199)
 w(642:650) = u(4)*w(191:199)
 w(651:652) = u(8)*w(48:49)
 w(653) = u(9)*w(49)
endif
! terms of degree 9
if (9.le.mxd) then
 w(654:939) = u(0)*w(368:653)
 w(940:1057) = u(1)*w(536:653)
 w(1058:1081) = u(2)*w(344:367)
 w(1082:1096) = u(3)*w(353:367)
 w(1097:1105) = u(4)*w(359:367)
 w(1106:1111) = u(5)*w(194:199)
 w(1112:1114) = u(6)*w(197:199)
 w(1115) = u(7)*w(199)
endif
END SUBROUTINE mg41_prib
