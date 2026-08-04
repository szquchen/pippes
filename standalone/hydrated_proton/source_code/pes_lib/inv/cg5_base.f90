SUBROUTINE cg5_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:cg5_nb(mxd)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:nr-1), v(0:cg5_nsc(mxd)-1)
if (size(x).ne.nr) then
 stop 'cg5_base: bad size x'
!! else if (size(w).ne.cg5_nb(mxd)) then
!!  stop 'cg5_base: bad size w'
endif
call cg5_prims (x, u)
call cg5_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 2 2 2 2 1 0 0 0
! secs,  dnsc(0:*): 1 0 0 2 5 8 15 23 33 46
! base,  dnb(0:*): 1 1 3 7 17 35 76 149 291 539
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
 w(10:11) = v(1:2)
endif
! terms of degree 4
if (4.le.mxd) then
 w(12:18) = u(0)*w(5:11)
 w(19:20) = u(1)*w(3:4)
 w(21) = u(2)*w(4)
 w(22) = u(5)*w(0)
 w(23) = u(6)*w(0)
 w(24:28) = v(3:7)
endif
! terms of degree 5
if (5.le.mxd) then
 w(29:45) = u(0)*w(12:28)
 w(46:49) = u(1)*w(8:11)
 w(50:53) = u(2)*w(8:11)
 w(54) = u(7)*w(0)
 w(55) = u(8)*w(0)
 w(56:63) = v(8:15)
endif
! terms of degree 6
if (6.le.mxd) then
 w(64:98) = u(0)*w(29:63)
 w(99:108) = u(1)*w(19:28)
 w(109:116) = u(2)*w(21:28)
 w(117:120) = u(3)*w(8:11)
 w(121:123) = u(4)*w(9:11)
 w(124) = u(9)*w(0)
 w(125:139) = v(16:30)
endif
! terms of degree 7
if (7.le.mxd) then
 w(140:215) = u(0)*w(64:139)
 w(216:233) = u(1)*w(46:63)
 w(234:247) = u(2)*w(50:63)
 w(248:254) = u(3)*w(22:28)
 w(255:261) = u(4)*w(22:28)
 w(262:263) = u(5)*w(10:11)
 w(264:265) = u(6)*w(10:11)
 w(266:288) = v(31:53)
endif
! terms of degree 8
if (8.le.mxd) then
 w(289:437) = u(0)*w(140:288)
 w(438:478) = u(1)*w(99:139)
 w(479:509) = u(2)*w(109:139)
 w(510:519) = u(3)*w(54:63)
 w(520:529) = u(4)*w(54:63)
 w(530:536) = u(5)*w(22:28)
 w(537:542) = u(6)*w(23:28)
 w(543:544) = u(7)*w(10:11)
 w(545:546) = u(8)*w(10:11)
 w(547:579) = v(54:86)
endif
! terms of degree 9
if (9.le.mxd) then
 w(580:870) = u(0)*w(289:579)
 w(871:943) = u(1)*w(216:288)
 w(944:998) = u(2)*w(234:288)
 w(999:1021) = u(3)*w(117:139)
 w(1022:1040) = u(4)*w(121:139)
 w(1041:1050) = u(5)*w(54:63)
 w(1051:1060) = u(6)*w(54:63)
 w(1061:1065) = u(7)*w(24:28)
 w(1066:1070) = u(8)*w(24:28)
 w(1071:1072) = u(9)*w(10:11)
 w(1073:1118) = v(87:132)
endif
END SUBROUTINE cg5_base
