SUBROUTINE cg6_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:cg6_nb(mxd)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:nr-1), v(0:cg6_nsc(mxd)-1)
if (size(x).ne.nr) then
 stop 'cg6_base: bad size x'
!! else if (size(w).ne.cg6_nb(mxd)) then
!!  stop 'cg6_base: bad size w'
endif
call cg6_prims (x, u)
call cg6_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 2 4 3 3 2 0 0 0
! secs,  dnsc(0:*): 1 0 0 1 7 18 40 81 167 328
! base,  dnb(0:*): 1 1 3 8 21 52 132 313 741 1684
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
 w(4) = u(2)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(5:7) = u(0)*w(2:4)
 w(8) = u(3)*w(0)
 w(9) = u(4)*w(0)
 w(10) = u(5)*w(0)
 w(11) = u(6)*w(0)
 w(12) = v(1)
endif
! terms of degree 4
if (4.le.mxd) then
 w(13:20) = u(0)*w(5:12)
 w(21:22) = u(1)*w(3:4)
 w(23) = u(2)*w(4)
 w(24) = u(7)*w(0)
 w(25) = u(8)*w(0)
 w(26) = u(9)*w(0)
 w(27:33) = v(2:8)
endif
! terms of degree 5
if (5.le.mxd) then
 w(34:54) = u(0)*w(13:33)
 w(55:59) = u(1)*w(8:12)
 w(60:64) = u(2)*w(8:12)
 w(65) = u(10)*w(0)
 w(66) = u(11)*w(0)
 w(67) = u(12)*w(0)
 w(68:85) = v(9:26)
endif
! terms of degree 6
if (6.le.mxd) then
 w(86:137) = u(0)*w(34:85)
 w(138:150) = u(1)*w(21:33)
 w(151:161) = u(2)*w(23:33)
 w(162:166) = u(3)*w(8:12)
 w(167:170) = u(4)*w(9:12)
 w(171:173) = u(5)*w(10:12)
 w(174:175) = u(6)*w(11:12)
 w(176) = u(13)*w(0)
 w(177) = u(14)*w(0)
 w(178:217) = v(27:66)
endif
! terms of degree 7
if (7.le.mxd) then
 w(218:349) = u(0)*w(86:217)
 w(350:380) = u(1)*w(55:85)
 w(381:406) = u(2)*w(60:85)
 w(407:416) = u(3)*w(24:33)
 w(417:426) = u(4)*w(24:33)
 w(427:436) = u(5)*w(24:33)
 w(437:446) = u(6)*w(24:33)
 w(447) = u(7)*w(12)
 w(448) = u(8)*w(12)
 w(449) = u(9)*w(12)
 w(450:530) = v(67:147)
endif
! terms of degree 8
if (8.le.mxd) then
 w(531:843) = u(0)*w(218:530)
 w(844:923) = u(1)*w(138:217)
 w(924:990) = u(2)*w(151:217)
 w(991:1011) = u(3)*w(65:85)
 w(1012:1032) = u(4)*w(65:85)
 w(1033:1053) = u(5)*w(65:85)
 w(1054:1074) = u(6)*w(65:85)
 w(1075:1084) = u(7)*w(24:33)
 w(1085:1093) = u(8)*w(25:33)
 w(1094:1101) = u(9)*w(26:33)
 w(1102) = u(10)*w(12)
 w(1103) = u(11)*w(12)
 w(1104) = u(12)*w(12)
 w(1105:1271) = v(148:314)
endif
! terms of degree 9
if (9.le.mxd) then
 w(1272:2012) = u(0)*w(531:1271)
 w(2013:2193) = u(1)*w(350:530)
 w(2194:2343) = u(2)*w(381:530)
 w(2344:2399) = u(3)*w(162:217)
 w(2400:2450) = u(4)*w(167:217)
 w(2451:2497) = u(5)*w(171:217)
 w(2498:2541) = u(6)*w(174:217)
 w(2542:2562) = u(7)*w(65:85)
 w(2563:2583) = u(8)*w(65:85)
 w(2584:2604) = u(9)*w(65:85)
 w(2605:2611) = u(10)*w(27:33)
 w(2612:2618) = u(11)*w(27:33)
 w(2619:2625) = u(12)*w(27:33)
 w(2626) = u(13)*w(12)
 w(2627) = u(14)*w(12)
 w(2628:2955) = v(315:642)
endif
END SUBROUTINE cg6_base
