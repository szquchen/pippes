SUBROUTINE bg72_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:bg72_nr-1), v(0:bg72_nsc(mxd)-1)
if (size(x).ne.bg72_nr.or.size(w).ne.bg72_nb(mxd)) then
 stop 'bg72_base: bad dimensions'
endif
call bg72_prims (x, u)
call bg72_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 3 2 2 1 2 1 0 0
! secs,  dnsc(0:*): 1 0 0 1 4 6 10 15 25 34
! base,  dnb(0:*): 1 1 4 7 19 35 80 149 302 549
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
 w(5) = u(3)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(6:9) = u(0)*w(2:5)
 w(10) = u(4)*w(0)
 w(11) = u(5)*w(0)
 w(12) = v(1)
endif
! terms of degree 4
if (4.le.mxd) then
 w(13:19) = u(0)*w(6:12)
 w(20:22) = u(1)*w(3:5)
 w(23:24) = u(2)*w(4:5)
 w(25) = u(3)*w(5)
 w(26) = u(6)*w(0)
 w(27) = u(7)*w(0)
 w(28:31) = v(2:5)
endif
! terms of degree 5
if (5.le.mxd) then
 w(32:50) = u(0)*w(13:31)
 w(51:53) = u(1)*w(10:12)
 w(54:56) = u(2)*w(10:12)
 w(57:59) = u(3)*w(10:12)
 w(60) = u(8)*w(0)
 w(61:66) = v(6:11)
endif
! terms of degree 6
if (6.le.mxd) then
 w(67:101) = u(0)*w(32:66)
 w(102:113) = u(1)*w(20:31)
 w(114:122) = u(2)*w(23:31)
 w(123:129) = u(3)*w(25:31)
 w(130:132) = u(4)*w(10:12)
 w(133:134) = u(5)*w(11:12)
 w(135) = u(9)*w(0)
 w(136) = u(10)*w(0)
 w(137:146) = v(12:21)
endif
! terms of degree 7
if (7.le.mxd) then
 w(147:226) = u(0)*w(67:146)
 w(227:242) = u(1)*w(51:66)
 w(243:255) = u(2)*w(54:66)
 w(256:265) = u(3)*w(57:66)
 w(266:271) = u(4)*w(26:31)
 w(272:277) = u(5)*w(26:31)
 w(278) = u(6)*w(12)
 w(279) = u(7)*w(12)
 w(280) = u(11)*w(0)
 w(281:295) = v(22:36)
endif
! terms of degree 8
if (8.le.mxd) then
 w(296:444) = u(0)*w(147:295)
 w(445:489) = u(1)*w(102:146)
 w(490:522) = u(2)*w(114:146)
 w(523:546) = u(3)*w(123:146)
 w(547:553) = u(4)*w(60:66)
 w(554:560) = u(5)*w(60:66)
 w(561:566) = u(6)*w(26:31)
 w(567:571) = u(7)*w(27:31)
 w(572) = u(8)*w(12)
 w(573:597) = v(37:61)
endif
! terms of degree 9
if (9.le.mxd) then
 w(598:899) = u(0)*w(296:597)
 w(900:968) = u(1)*w(227:295)
 w(969:1021) = u(2)*w(243:295)
 w(1022:1061) = u(3)*w(256:295)
 w(1062:1078) = u(4)*w(130:146)
 w(1079:1092) = u(5)*w(133:146)
 w(1093:1099) = u(6)*w(60:66)
 w(1100:1106) = u(7)*w(60:66)
 w(1107:1110) = u(8)*w(28:31)
 w(1111) = u(9)*w(12)
 w(1112) = u(10)*w(12)
 w(1113:1146) = v(62:95)
endif
END SUBROUTINE bg72_base
