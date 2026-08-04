SUBROUTINE bg52_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:bg52_nr-1), v(0:bg52_nsc(mxd)-1)
if (size(x).ne.bg52_nr.or.size(w).ne.bg52_nb(mxd)) then
 stop 'bg52_base: bad dimensions'
endif
call bg52_prims (x, u)
call bg52_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 3 1 2 1 1 0 0 0
! secs,  dnsc(0:*): 1 0 0 2 4 6 9 11 17 21
! base,  dnb(0:*): 1 1 4 7 19 35 76 136 263 452
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
 w(11:12) = v(1:2)
endif
! terms of degree 4
if (4.le.mxd) then
 w(13:19) = u(0)*w(6:12)
 w(20:22) = u(1)*w(3:5)
 w(23:24) = u(2)*w(4:5)
 w(25) = u(3)*w(5)
 w(26) = u(5)*w(0)
 w(27) = u(6)*w(0)
 w(28:31) = v(3:6)
endif
! terms of degree 5
if (5.le.mxd) then
 w(32:50) = u(0)*w(13:31)
 w(51:53) = u(1)*w(10:12)
 w(54:56) = u(2)*w(10:12)
 w(57:59) = u(3)*w(10:12)
 w(60) = u(7)*w(0)
 w(61:66) = v(7:12)
endif
! terms of degree 6
if (6.le.mxd) then
 w(67:101) = u(0)*w(32:66)
 w(102:113) = u(1)*w(20:31)
 w(114:122) = u(2)*w(23:31)
 w(123:129) = u(3)*w(25:31)
 w(130:132) = u(4)*w(10:12)
 w(133) = u(8)*w(0)
 w(134:142) = v(13:21)
endif
! terms of degree 7
if (7.le.mxd) then
 w(143:218) = u(0)*w(67:142)
 w(219:234) = u(1)*w(51:66)
 w(235:247) = u(2)*w(54:66)
 w(248:257) = u(3)*w(57:66)
 w(258:263) = u(4)*w(26:31)
 w(264:265) = u(5)*w(11:12)
 w(266:267) = u(6)*w(11:12)
 w(268:278) = v(22:32)
endif
! terms of degree 8
if (8.le.mxd) then
 w(279:414) = u(0)*w(143:278)
 w(415:455) = u(1)*w(102:142)
 w(456:484) = u(2)*w(114:142)
 w(485:504) = u(3)*w(123:142)
 w(505:511) = u(4)*w(60:66)
 w(512:517) = u(5)*w(26:31)
 w(518:522) = u(6)*w(27:31)
 w(523:524) = u(7)*w(11:12)
 w(525:541) = v(33:49)
endif
! terms of degree 9
if (9.le.mxd) then
 w(542:804) = u(0)*w(279:541)
 w(805:864) = u(1)*w(219:278)
 w(865:908) = u(2)*w(235:278)
 w(909:939) = u(3)*w(248:278)
 w(940:952) = u(4)*w(130:142)
 w(953:959) = u(5)*w(60:66)
 w(960:966) = u(6)*w(60:66)
 w(967:970) = u(7)*w(28:31)
 w(971:972) = u(8)*w(11:12)
 w(973:993) = v(50:70)
endif
END SUBROUTINE bg52_base
