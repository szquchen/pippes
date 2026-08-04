SUBROUTINE mg22_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg22_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg22_prib: bad size u'
!! else if (size(w).ne.mg22_npb(mxd)) then
!!  stop 'mg22_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 3 3 0 0 0 0 0 0 0 &
!   0 0 0 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 3 9 19 39 69 119 189 294 434 &
!   630 882 1218 1638 2178 2838 3663 4653 5863 7293
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
 w(10) = u(3)*w(0)
 w(11) = u(4)*w(0)
 w(12) = u(5)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(13:21) = u(0)*w(4:12)
 w(22:27) = u(1)*w(7:12)
 w(28:31) = u(2)*w(9:12)
endif
! terms of degree 4
if (4.le.mxd) then
 w(32:50) = u(0)*w(13:31)
 w(51:60) = u(1)*w(22:31)
 w(61:64) = u(2)*w(28:31)
 w(65:67) = u(3)*w(10:12)
 w(68:69) = u(4)*w(11:12)
 w(70) = u(5)*w(12)
endif
! terms of degree 5
if (5.le.mxd) then
 w(71:109) = u(0)*w(32:70)
 w(110:129) = u(1)*w(51:70)
 w(130:139) = u(2)*w(61:70)
endif
! terms of degree 6
if (6.le.mxd) then
 w(140:208) = u(0)*w(71:139)
 w(209:238) = u(1)*w(110:139)
 w(239:248) = u(2)*w(130:139)
 w(249:254) = u(3)*w(65:70)
 w(255:257) = u(4)*w(68:70)
 w(258) = u(5)*w(70)
endif
! terms of degree 7
if (7.le.mxd) then
 w(259:377) = u(0)*w(140:258)
 w(378:427) = u(1)*w(209:258)
 w(428:447) = u(2)*w(239:258)
endif
! terms of degree 8
if (8.le.mxd) then
 w(448:636) = u(0)*w(259:447)
 w(637:706) = u(1)*w(378:447)
 w(707:726) = u(2)*w(428:447)
 w(727:736) = u(3)*w(249:258)
 w(737:740) = u(4)*w(255:258)
 w(741) = u(5)*w(258)
endif
! terms of degree 9
if (9.le.mxd) then
 w(742:1035) = u(0)*w(448:741)
 w(1036:1140) = u(1)*w(637:741)
 w(1141:1175) = u(2)*w(707:741)
endif
! terms of degree 10
if (10.le.mxd) then
 w(1176:1609) = u(0)*w(742:1175)
 w(1610:1749) = u(1)*w(1036:1175)
 w(1750:1784) = u(2)*w(1141:1175)
 w(1785:1799) = u(3)*w(727:741)
 w(1800:1804) = u(4)*w(737:741)
 w(1805) = u(5)*w(741)
endif
! terms of degree 11
if (11.le.mxd) then
 w(1806:2435) = u(0)*w(1176:1805)
 w(2436:2631) = u(1)*w(1610:1805)
 w(2632:2687) = u(2)*w(1750:1805)
endif
! terms of degree 12
if (12.le.mxd) then
 w(2688:3569) = u(0)*w(1806:2687)
 w(3570:3821) = u(1)*w(2436:2687)
 w(3822:3877) = u(2)*w(2632:2687)
 w(3878:3898) = u(3)*w(1785:1805)
 w(3899:3904) = u(4)*w(1800:1805)
 w(3905) = u(5)*w(1805)
endif
! terms of degree 13
if (13.le.mxd) then
 w(3906:5123) = u(0)*w(2688:3905)
 w(5124:5459) = u(1)*w(3570:3905)
 w(5460:5543) = u(2)*w(3822:3905)
endif
! terms of degree 14
if (14.le.mxd) then
 w(5544:7181) = u(0)*w(3906:5543)
 w(7182:7601) = u(1)*w(5124:5543)
 w(7602:7685) = u(2)*w(5460:5543)
 w(7686:7713) = u(3)*w(3878:3905)
 w(7714:7720) = u(4)*w(3899:3905)
 w(7721) = u(5)*w(3905)
endif
! terms of degree 15
if (15.le.mxd) then
 w(7722:9899) = u(0)*w(5544:7721)
 w(9900:10439) = u(1)*w(7182:7721)
 w(10440:10559) = u(2)*w(7602:7721)
endif
! terms of degree 16
if (16.le.mxd) then
 w(10560:13397) = u(0)*w(7722:10559)
 w(13398:14057) = u(1)*w(9900:10559)
 w(14058:14177) = u(2)*w(10440:10559)
 w(14178:14213) = u(3)*w(7686:7721)
 w(14214:14221) = u(4)*w(7714:7721)
 w(14222) = u(5)*w(7721)
endif
! terms of degree 17
if (17.le.mxd) then
 w(14223:17885) = u(0)*w(10560:14222)
 w(17886:18710) = u(1)*w(13398:14222)
 w(18711:18875) = u(2)*w(14058:14222)
endif
! terms of degree 18
if (18.le.mxd) then
 w(18876:23528) = u(0)*w(14223:18875)
 w(23529:24518) = u(1)*w(17886:18875)
 w(24519:24683) = u(2)*w(18711:18875)
 w(24684:24728) = u(3)*w(14178:14222)
 w(24729:24737) = u(4)*w(14214:14222)
 w(24738) = u(5)*w(14222)
endif
! terms of degree 19
if (19.le.mxd) then
 w(24739:30601) = u(0)*w(18876:24738)
 w(30602:31811) = u(1)*w(23529:24738)
 w(31812:32031) = u(2)*w(24519:24738)
endif
END SUBROUTINE mg22_prib
