SUBROUTINE cg7_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:cg7_nb(mxd)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:nr-1), v(0:cg7_nsc(mxd)-1)
if (size(x).ne.nr) then
 stop 'cg7_base: bad size x'
!! else if (size(w).ne.cg7_nb(mxd)) then
!!  stop 'cg7_base: bad size w'
endif
call cg7_prims (x, u)
call cg7_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 2 5 3 3 2 3 0 0
! secs,  dnsc(0:*): 1 0 0 0 8 25 70 169 450 1146
! base,  dnb(0:*): 1 1 3 8 22 60 173 471 1303 3510
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
 w(12) = u(7)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(13:20) = u(0)*w(5:12)
 w(21:22) = u(1)*w(3:4)
 w(23) = u(2)*w(4)
 w(24) = u(8)*w(0)
 w(25) = u(9)*w(0)
 w(26) = u(10)*w(0)
 w(27:34) = v(1:8)
endif
! terms of degree 5
if (5.le.mxd) then
 w(35:56) = u(0)*w(13:34)
 w(57:61) = u(1)*w(8:12)
 w(62:66) = u(2)*w(8:12)
 w(67) = u(11)*w(0)
 w(68) = u(12)*w(0)
 w(69) = u(13)*w(0)
 w(70:94) = v(9:33)
endif
! terms of degree 6
if (6.le.mxd) then
 w(95:154) = u(0)*w(35:94)
 w(155:168) = u(1)*w(21:34)
 w(169:180) = u(2)*w(23:34)
 w(181:185) = u(3)*w(8:12)
 w(186:189) = u(4)*w(9:12)
 w(190:192) = u(5)*w(10:12)
 w(193:194) = u(6)*w(11:12)
 w(195) = u(7)*w(12)
 w(196) = u(14)*w(0)
 w(197) = u(15)*w(0)
 w(198:267) = v(34:103)
endif
! terms of degree 7
if (7.le.mxd) then
 w(268:440) = u(0)*w(95:267)
 w(441:478) = u(1)*w(57:94)
 w(479:511) = u(2)*w(62:94)
 w(512:522) = u(3)*w(24:34)
 w(523:533) = u(4)*w(24:34)
 w(534:544) = u(5)*w(24:34)
 w(545:555) = u(6)*w(24:34)
 w(556:566) = u(7)*w(24:34)
 w(567) = u(16)*w(0)
 w(568) = u(17)*w(0)
 w(569) = u(18)*w(0)
 w(570:738) = v(104:272)
endif
! terms of degree 8
if (8.le.mxd) then
 w(739:1209) = u(0)*w(268:738)
 w(1210:1322) = u(1)*w(155:267)
 w(1323:1421) = u(2)*w(169:267)
 w(1422:1449) = u(3)*w(67:94)
 w(1450:1477) = u(4)*w(67:94)
 w(1478:1505) = u(5)*w(67:94)
 w(1506:1533) = u(6)*w(67:94)
 w(1534:1561) = u(7)*w(67:94)
 w(1562:1572) = u(8)*w(24:34)
 w(1573:1582) = u(9)*w(25:34)
 w(1583:1591) = u(10)*w(26:34)
 w(1592:2041) = v(273:722)
endif
! terms of degree 9
if (9.le.mxd) then
 w(2042:3344) = u(0)*w(739:2041)
 w(3345:3642) = u(1)*w(441:738)
 w(3643:3902) = u(2)*w(479:738)
 w(3903:3989) = u(3)*w(181:267)
 w(3990:4071) = u(4)*w(186:267)
 w(4072:4149) = u(5)*w(190:267)
 w(4150:4224) = u(6)*w(193:267)
 w(4225:4297) = u(7)*w(195:267)
 w(4298:4325) = u(8)*w(67:94)
 w(4326:4353) = u(9)*w(67:94)
 w(4354:4381) = u(10)*w(67:94)
 w(4382:4389) = u(11)*w(27:34)
 w(4390:4397) = u(12)*w(27:34)
 w(4398:4405) = u(13)*w(27:34)
 w(4406:5551) = v(723:1868)
endif
END SUBROUTINE cg7_base
