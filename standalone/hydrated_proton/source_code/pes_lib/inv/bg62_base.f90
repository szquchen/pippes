SUBROUTINE bg62_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:bg62_nr-1), v(0:bg62_nsc(mxd)-1)
if (size(x).ne.bg62_nr.or.size(w).ne.bg62_nb(mxd)) then
 stop 'bg62_base: bad dimensions'
endif
call bg62_prims (x, u)
call bg62_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 3 2 2 1 2 0 0 0
! secs,  dnsc(0:*): 1 0 0 1 4 6 10 12 18 25
! base,  dnb(0:*): 1 1 4 7 19 35 80 145 291 517
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
 w(280:291) = v(22:33)
endif
! terms of degree 8
if (8.le.mxd) then
 w(292:436) = u(0)*w(147:291)
 w(437:481) = u(1)*w(102:146)
 w(482:514) = u(2)*w(114:146)
 w(515:538) = u(3)*w(123:146)
 w(539:545) = u(4)*w(60:66)
 w(546:552) = u(5)*w(60:66)
 w(553:558) = u(6)*w(26:31)
 w(559:563) = u(7)*w(27:31)
 w(564) = u(8)*w(12)
 w(565:582) = v(34:51)
endif
! terms of degree 9
if (9.le.mxd) then
 w(583:873) = u(0)*w(292:582)
 w(874:938) = u(1)*w(227:291)
 w(939:987) = u(2)*w(243:291)
 w(988:1023) = u(3)*w(256:291)
 w(1024:1040) = u(4)*w(130:146)
 w(1041:1054) = u(5)*w(133:146)
 w(1055:1061) = u(6)*w(60:66)
 w(1062:1068) = u(7)*w(60:66)
 w(1069:1072) = u(8)*w(28:31)
 w(1073) = u(9)*w(12)
 w(1074) = u(10)*w(12)
 w(1075:1099) = v(52:76)
endif
END SUBROUTINE bg62_base
