SUBROUTINE mg4_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg4_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg4_prib: bad size u'
!! else if (size(w).ne.mg4_npb(mxd)) then
!!  stop 'mg4_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 2 2 1 0 0 0 0 0 &
!   0 0 0 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 1 3 5 9 13 22 30 45 61 &
!   85 111 150 190 247 309 390 478 593 715
! constant term
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(2) = u(0)*w(1)
 w(3) = u(1)*w(0)
 w(4) = u(2)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(5:7) = u(0)*w(2:4)
 w(8) = u(3)*w(0)
 w(9) = u(4)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(10:14) = u(0)*w(5:9)
 w(15:16) = u(1)*w(3:4)
 w(17) = u(2)*w(4)
 w(18) = u(5)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(19:27) = u(0)*w(10:18)
 w(28:29) = u(1)*w(8:9)
 w(30:31) = u(2)*w(8:9)
endif
! terms of degree 6
if (6.le.mxd) then
 w(32:44) = u(0)*w(19:31)
 w(45:48) = u(1)*w(15:18)
 w(49:50) = u(2)*w(17:18)
 w(51:52) = u(3)*w(8:9)
 w(53) = u(4)*w(9)
endif
! terms of degree 7
if (7.le.mxd) then
 w(54:75) = u(0)*w(32:53)
 w(76:79) = u(1)*w(28:31)
 w(80:81) = u(2)*w(30:31)
 w(82) = u(3)*w(18)
 w(83) = u(4)*w(18)
endif
! terms of degree 8
if (8.le.mxd) then
 w(84:113) = u(0)*w(54:83)
 w(114:122) = u(1)*w(45:53)
 w(123:127) = u(2)*w(49:53)
 w(128) = u(5)*w(18)
endif
! terms of degree 9
if (9.le.mxd) then
 w(129:173) = u(0)*w(84:128)
 w(174:181) = u(1)*w(76:83)
 w(182:185) = u(2)*w(80:83)
 w(186:188) = u(3)*w(51:53)
 w(189) = u(4)*w(53)
endif
! terms of degree 10
if (10.le.mxd) then
 w(190:250) = u(0)*w(129:189)
 w(251:265) = u(1)*w(114:128)
 w(266:271) = u(2)*w(123:128)
 w(272:273) = u(3)*w(82:83)
 w(274) = u(4)*w(83)
endif
! terms of degree 11
if (11.le.mxd) then
 w(275:359) = u(0)*w(190:274)
 w(360:375) = u(1)*w(174:189)
 w(376:383) = u(2)*w(182:189)
 w(384) = u(3)*w(128)
 w(385) = u(4)*w(128)
endif
! terms of degree 12
if (12.le.mxd) then
 w(386:496) = u(0)*w(275:385)
 w(497:520) = u(1)*w(251:274)
 w(521:529) = u(2)*w(266:274)
 w(530:533) = u(3)*w(186:189)
 w(534) = u(4)*w(189)
 w(535) = u(5)*w(128)
endif
! terms of degree 13
if (13.le.mxd) then
 w(536:685) = u(0)*w(386:535)
 w(686:711) = u(1)*w(360:385)
 w(712:721) = u(2)*w(376:385)
 w(722:724) = u(3)*w(272:274)
 w(725) = u(4)*w(274)
endif
! terms of degree 14
if (14.le.mxd) then
 w(726:915) = u(0)*w(536:725)
 w(916:954) = u(1)*w(497:535)
 w(955:969) = u(2)*w(521:535)
 w(970:971) = u(3)*w(384:385)
 w(972) = u(4)*w(385)
endif
! terms of degree 15
if (15.le.mxd) then
 w(973:1219) = u(0)*w(726:972)
 w(1220:1259) = u(1)*w(686:725)
 w(1260:1273) = u(2)*w(712:725)
 w(1274:1279) = u(3)*w(530:535)
 w(1280:1281) = u(4)*w(534:535)
endif
! terms of degree 16
if (16.le.mxd) then
 w(1282:1590) = u(0)*w(973:1281)
 w(1591:1647) = u(1)*w(916:972)
 w(1648:1665) = u(2)*w(955:972)
 w(1666:1669) = u(3)*w(722:725)
 w(1670) = u(4)*w(725)
 w(1671) = u(5)*w(535)
endif
! terms of degree 17
if (17.le.mxd) then
 w(1672:2061) = u(0)*w(1282:1671)
 w(2062:2123) = u(1)*w(1220:1281)
 w(2124:2145) = u(2)*w(1260:1281)
 w(2146:2148) = u(3)*w(970:972)
 w(2149) = u(4)*w(972)
endif
! terms of degree 18
if (18.le.mxd) then
 w(2150:2627) = u(0)*w(1672:2149)
 w(2628:2708) = u(1)*w(1591:1671)
 w(2709:2732) = u(2)*w(1648:1671)
 w(2733:2740) = u(3)*w(1274:1281)
 w(2741:2742) = u(4)*w(1280:1281)
endif
! terms of degree 19
if (19.le.mxd) then
 w(2743:3335) = u(0)*w(2150:2742)
 w(3336:3423) = u(1)*w(2062:2149)
 w(3424:3449) = u(2)*w(2124:2149)
 w(3450:3455) = u(3)*w(1666:1671)
 w(3456:3457) = u(4)*w(1670:1671)
endif
END SUBROUTINE mg4_prib
