SUBROUTINE mg6_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg6_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg6_prib: bad size u'
!! else if (size(w).ne.mg6_npb(mxd)) then
!!  stop 'mg6_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 2 4 3 3 2 0 0 0
! prib,  dnpb(0:*): 1 1 3 7 13 24 46 76 132 218
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
 w(10) = u(5)*w(0)
 w(11) = u(6)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(12:18) = u(0)*w(5:11)
 w(19:20) = u(1)*w(3:4)
 w(21) = u(2)*w(4)
 w(22) = u(7)*w(0)
 w(23) = u(8)*w(0)
 w(24) = u(9)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(25:37) = u(0)*w(12:24)
 w(38:41) = u(1)*w(8:11)
 w(42:45) = u(2)*w(8:11)
 w(46) = u(10)*w(0)
 w(47) = u(11)*w(0)
 w(48) = u(12)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(49:72) = u(0)*w(25:48)
 w(73:78) = u(1)*w(19:24)
 w(79:82) = u(2)*w(21:24)
 w(83:86) = u(3)*w(8:11)
 w(87:89) = u(4)*w(9:11)
 w(90:91) = u(5)*w(10:11)
 w(92) = u(6)*w(11)
 w(93) = u(13)*w(0)
 w(94) = u(14)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(95:140) = u(0)*w(49:94)
 w(141:151) = u(1)*w(38:48)
 w(152:158) = u(2)*w(42:48)
 w(159:161) = u(3)*w(22:24)
 w(162:164) = u(4)*w(22:24)
 w(165:167) = u(5)*w(22:24)
 w(168:170) = u(6)*w(22:24)
endif
! terms of degree 8
if (8.le.mxd) then
 w(171:246) = u(0)*w(95:170)
 w(247:268) = u(1)*w(73:94)
 w(269:284) = u(2)*w(79:94)
 w(285:287) = u(3)*w(46:48)
 w(288:290) = u(4)*w(46:48)
 w(291:293) = u(5)*w(46:48)
 w(294:296) = u(6)*w(46:48)
 w(297:299) = u(7)*w(22:24)
 w(300:301) = u(8)*w(23:24)
 w(302) = u(9)*w(24)
endif
! terms of degree 9
if (9.le.mxd) then
 w(303:434) = u(0)*w(171:302)
 w(435:464) = u(1)*w(141:170)
 w(465:483) = u(2)*w(152:170)
 w(484:495) = u(3)*w(83:94)
 w(496:503) = u(4)*w(87:94)
 w(504:508) = u(5)*w(90:94)
 w(509:511) = u(6)*w(92:94)
 w(512:514) = u(7)*w(46:48)
 w(515:517) = u(8)*w(46:48)
 w(518:520) = u(9)*w(46:48)
endif
END SUBROUTINE mg6_prib
