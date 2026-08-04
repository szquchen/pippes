SUBROUTINE mg32_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg32_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg32_prib: bad size u'
!! else if (size(w).ne.mg32_npb(mxd)) then
!!  stop 'mg32_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 3 4 2 0 0 1 0 0 0
! prib,  dnpb(0:*): 1 3 10 24 55 111 216 390 684 1144
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
 w(13) = u(6)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(14:23) = u(0)*w(4:13)
 w(24:30) = u(1)*w(7:13)
 w(31:35) = u(2)*w(9:13)
 w(36) = u(7)*w(0)
 w(37) = u(8)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(38:61) = u(0)*w(14:37)
 w(62:75) = u(1)*w(24:37)
 w(76:82) = u(2)*w(31:37)
 w(83:86) = u(3)*w(10:13)
 w(87:89) = u(4)*w(11:13)
 w(90:91) = u(5)*w(12:13)
 w(92) = u(6)*w(13)
endif
! terms of degree 5
if (5.le.mxd) then
 w(93:147) = u(0)*w(38:92)
 w(148:178) = u(1)*w(62:92)
 w(179:195) = u(2)*w(76:92)
 w(196:197) = u(3)*w(36:37)
 w(198:199) = u(4)*w(36:37)
 w(200:201) = u(5)*w(36:37)
 w(202:203) = u(6)*w(36:37)
endif
! terms of degree 6
if (6.le.mxd) then
 w(204:314) = u(0)*w(93:203)
 w(315:370) = u(1)*w(148:203)
 w(371:395) = u(2)*w(179:203)
 w(396:405) = u(3)*w(83:92)
 w(406:411) = u(4)*w(87:92)
 w(412:414) = u(5)*w(90:92)
 w(415) = u(6)*w(92)
 w(416:417) = u(7)*w(36:37)
 w(418) = u(8)*w(37)
 w(419) = u(9)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(420:635) = u(0)*w(204:419)
 w(636:740) = u(1)*w(315:419)
 w(741:789) = u(2)*w(371:419)
 w(790:797) = u(3)*w(196:203)
 w(798:803) = u(4)*w(198:203)
 w(804:807) = u(5)*w(200:203)
 w(808:809) = u(6)*w(202:203)
endif
! terms of degree 8
if (8.le.mxd) then
 w(810:1199) = u(0)*w(420:809)
 w(1200:1373) = u(1)*w(636:809)
 w(1374:1442) = u(2)*w(741:809)
 w(1443:1466) = u(3)*w(396:419)
 w(1467:1480) = u(4)*w(406:419)
 w(1481:1488) = u(5)*w(412:419)
 w(1489:1493) = u(6)*w(415:419)
endif
! terms of degree 9
if (9.le.mxd) then
 w(1494:2177) = u(0)*w(810:1493)
 w(2178:2471) = u(1)*w(1200:1493)
 w(2472:2591) = u(2)*w(1374:1493)
 w(2592:2611) = u(3)*w(790:809)
 w(2612:2623) = u(4)*w(798:809)
 w(2624:2629) = u(5)*w(804:809)
 w(2630:2631) = u(6)*w(808:809)
 w(2632:2635) = u(7)*w(416:419)
 w(2636:2637) = u(8)*w(418:419)
endif
END SUBROUTINE mg32_prib
