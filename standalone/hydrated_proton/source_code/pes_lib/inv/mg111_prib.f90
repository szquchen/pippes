SUBROUTINE mg111_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg111_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg111_prib: bad size u'
!! else if (size(w).ne.mg111_npb(mxd)) then
!!  stop 'mg111_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 3 0 0 0 0 0 0 0 0 &
!   0 0 0 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 3 6 10 15 21 28 36 45 55 &
!   66 78 91 105 120 136 153 171 190 210
! constant term
w(0) = 1
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
endif
! terms of degree 3
if (3.le.mxd) then
 w(10:15) = u(0)*w(4:9)
 w(16:18) = u(1)*w(7:9)
 w(19) = u(2)*w(9)
endif
! terms of degree 4
if (4.le.mxd) then
 w(20:29) = u(0)*w(10:19)
 w(30:33) = u(1)*w(16:19)
 w(34) = u(2)*w(19)
endif
! terms of degree 5
if (5.le.mxd) then
 w(35:49) = u(0)*w(20:34)
 w(50:54) = u(1)*w(30:34)
 w(55) = u(2)*w(34)
endif
! terms of degree 6
if (6.le.mxd) then
 w(56:76) = u(0)*w(35:55)
 w(77:82) = u(1)*w(50:55)
 w(83) = u(2)*w(55)
endif
! terms of degree 7
if (7.le.mxd) then
 w(84:111) = u(0)*w(56:83)
 w(112:118) = u(1)*w(77:83)
 w(119) = u(2)*w(83)
endif
! terms of degree 8
if (8.le.mxd) then
 w(120:155) = u(0)*w(84:119)
 w(156:163) = u(1)*w(112:119)
 w(164) = u(2)*w(119)
endif
! terms of degree 9
if (9.le.mxd) then
 w(165:209) = u(0)*w(120:164)
 w(210:218) = u(1)*w(156:164)
 w(219) = u(2)*w(164)
endif
! terms of degree 10
if (10.le.mxd) then
 w(220:274) = u(0)*w(165:219)
 w(275:284) = u(1)*w(210:219)
 w(285) = u(2)*w(219)
endif
! terms of degree 11
if (11.le.mxd) then
 w(286:351) = u(0)*w(220:285)
 w(352:362) = u(1)*w(275:285)
 w(363) = u(2)*w(285)
endif
! terms of degree 12
if (12.le.mxd) then
 w(364:441) = u(0)*w(286:363)
 w(442:453) = u(1)*w(352:363)
 w(454) = u(2)*w(363)
endif
! terms of degree 13
if (13.le.mxd) then
 w(455:545) = u(0)*w(364:454)
 w(546:558) = u(1)*w(442:454)
 w(559) = u(2)*w(454)
endif
! terms of degree 14
if (14.le.mxd) then
 w(560:664) = u(0)*w(455:559)
 w(665:678) = u(1)*w(546:559)
 w(679) = u(2)*w(559)
endif
! terms of degree 15
if (15.le.mxd) then
 w(680:799) = u(0)*w(560:679)
 w(800:814) = u(1)*w(665:679)
 w(815) = u(2)*w(679)
endif
! terms of degree 16
if (16.le.mxd) then
 w(816:951) = u(0)*w(680:815)
 w(952:967) = u(1)*w(800:815)
 w(968) = u(2)*w(815)
endif
! terms of degree 17
if (17.le.mxd) then
 w(969:1121) = u(0)*w(816:968)
 w(1122:1138) = u(1)*w(952:968)
 w(1139) = u(2)*w(968)
endif
! terms of degree 18
if (18.le.mxd) then
 w(1140:1310) = u(0)*w(969:1139)
 w(1311:1328) = u(1)*w(1122:1139)
 w(1329) = u(2)*w(1139)
endif
! terms of degree 19
if (19.le.mxd) then
 w(1330:1519) = u(0)*w(1140:1329)
 w(1520:1538) = u(1)*w(1311:1329)
 w(1539) = u(2)*w(1329)
endif
END SUBROUTINE mg111_prib
