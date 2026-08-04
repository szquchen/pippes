SUBROUTINE mg51_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg51_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg51_prib: bad size u'
!! else if (size(w).ne.mg51_npb(mxd)) then
!!  stop 'mg51_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 2 3 3 3 3 1 0 0 0
! prib,  dnpb(0:*): 1 2 6 13 29 57 111 201 360 616
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
 w(50) = u(10)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(51:79) = u(0)*w(22:50)
 w(80:95) = u(1)*w(35:50)
 w(96:98) = u(2)*w(19:21)
 w(99:101) = u(3)*w(19:21)
 w(102:104) = u(4)*w(19:21)
 w(105) = u(11)*w(0)
 w(106) = u(12)*w(0)
 w(107) = u(13)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(108:164) = u(0)*w(51:107)
 w(165:192) = u(1)*w(80:107)
 w(193:201) = u(2)*w(42:50)
 w(202:207) = u(3)*w(45:50)
 w(208:211) = u(4)*w(47:50)
 w(212:214) = u(5)*w(19:21)
 w(215:216) = u(6)*w(20:21)
 w(217) = u(7)*w(21)
 w(218) = u(14)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(219:329) = u(0)*w(108:218)
 w(330:383) = u(1)*w(165:218)
 w(384:395) = u(2)*w(96:107)
 w(396:404) = u(3)*w(99:107)
 w(405:410) = u(4)*w(102:107)
 w(411:413) = u(5)*w(48:50)
 w(414:416) = u(6)*w(48:50)
 w(417:419) = u(7)*w(48:50)
endif
! terms of degree 8
if (8.le.mxd) then
 w(420:620) = u(0)*w(219:419)
 w(621:710) = u(1)*w(330:419)
 w(711:736) = u(2)*w(193:218)
 w(737:753) = u(3)*w(202:218)
 w(754:764) = u(4)*w(208:218)
 w(765:767) = u(5)*w(105:107)
 w(768:770) = u(6)*w(105:107)
 w(771:773) = u(7)*w(105:107)
 w(774:776) = u(8)*w(48:50)
 w(777:778) = u(9)*w(49:50)
 w(779) = u(10)*w(50)
endif
! terms of degree 9
if (9.le.mxd) then
 w(780:1139) = u(0)*w(420:779)
 w(1140:1298) = u(1)*w(621:779)
 w(1299:1334) = u(2)*w(384:419)
 w(1335:1358) = u(3)*w(396:419)
 w(1359:1373) = u(4)*w(405:419)
 w(1374:1380) = u(5)*w(212:218)
 w(1381:1384) = u(6)*w(215:218)
 w(1385:1386) = u(7)*w(217:218)
 w(1387:1389) = u(8)*w(105:107)
 w(1390:1392) = u(9)*w(105:107)
 w(1393:1395) = u(10)*w(105:107)
endif
END SUBROUTINE mg51_prib
