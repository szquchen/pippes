SUBROUTINE mg5_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg5_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg5_prib: bad size u'
!! else if (size(w).ne.mg5_npb(mxd)) then
!!  stop 'mg5_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 2 2 2 2 1 0 0 0
! prib,  dnpb(0:*): 1 1 3 5 10 16 28 42 68 100
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
 w(19) = u(6)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(20:29) = u(0)*w(10:19)
 w(30:31) = u(1)*w(8:9)
 w(32:33) = u(2)*w(8:9)
 w(34) = u(7)*w(0)
 w(35) = u(8)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(36:51) = u(0)*w(20:35)
 w(52:56) = u(1)*w(15:19)
 w(57:59) = u(2)*w(17:19)
 w(60:61) = u(3)*w(8:9)
 w(62) = u(4)*w(9)
 w(63) = u(9)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(64:91) = u(0)*w(36:63)
 w(92:97) = u(1)*w(30:35)
 w(98:101) = u(2)*w(32:35)
 w(102:103) = u(3)*w(18:19)
 w(104:105) = u(4)*w(18:19)
endif
! terms of degree 8
if (8.le.mxd) then
 w(106:147) = u(0)*w(64:105)
 w(148:159) = u(1)*w(52:63)
 w(160:166) = u(2)*w(57:63)
 w(167:168) = u(3)*w(34:35)
 w(169:170) = u(4)*w(34:35)
 w(171:172) = u(5)*w(18:19)
 w(173) = u(6)*w(19)
endif
! terms of degree 9
if (9.le.mxd) then
 w(174:241) = u(0)*w(106:173)
 w(242:255) = u(1)*w(92:105)
 w(256:263) = u(2)*w(98:105)
 w(264:267) = u(3)*w(60:63)
 w(268:269) = u(4)*w(62:63)
 w(270:271) = u(5)*w(34:35)
 w(272:273) = u(6)*w(34:35)
endif
END SUBROUTINE mg5_prib
