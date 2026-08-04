SUBROUTINE cg7_prib (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:cg7_npb(mxd)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:nr-1)
if (size(x).ne.nr) then
 stop 'cg7_prib: bad size x'
!! else if (size(w).ne.cg7_npb(mxd)) then
!!  stop 'cg7_prib: bad size w'
endif
call cg7_prims (x, u)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 2 5 3 3 2 3 0 0
! prib,  dnpb(0:*): 1 1 3 8 14 27 54 93 162 281
! empty product
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
endif
! terms of degree 5
if (5.le.mxd) then
 w(27:40) = u(0)*w(13:26)
 w(41:45) = u(1)*w(8:12)
 w(46:50) = u(2)*w(8:12)
 w(51) = u(11)*w(0)
 w(52) = u(12)*w(0)
 w(53) = u(13)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(54:80) = u(0)*w(27:53)
 w(81:86) = u(1)*w(21:26)
 w(87:90) = u(2)*w(23:26)
 w(91:95) = u(3)*w(8:12)
 w(96:99) = u(4)*w(9:12)
 w(100:102) = u(5)*w(10:12)
 w(103:104) = u(6)*w(11:12)
 w(105) = u(7)*w(12)
 w(106) = u(14)*w(0)
 w(107) = u(15)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(108:161) = u(0)*w(54:107)
 w(162:174) = u(1)*w(41:53)
 w(175:182) = u(2)*w(46:53)
 w(183:185) = u(3)*w(24:26)
 w(186:188) = u(4)*w(24:26)
 w(189:191) = u(5)*w(24:26)
 w(192:194) = u(6)*w(24:26)
 w(195:197) = u(7)*w(24:26)
 w(198) = u(16)*w(0)
 w(199) = u(17)*w(0)
 w(200) = u(18)*w(0)
endif
! terms of degree 8
if (8.le.mxd) then
 w(201:293) = u(0)*w(108:200)
 w(294:320) = u(1)*w(81:107)
 w(321:341) = u(2)*w(87:107)
 w(342:344) = u(3)*w(51:53)
 w(345:347) = u(4)*w(51:53)
 w(348:350) = u(5)*w(51:53)
 w(351:353) = u(6)*w(51:53)
 w(354:356) = u(7)*w(51:53)
 w(357:359) = u(8)*w(24:26)
 w(360:361) = u(9)*w(25:26)
 w(362) = u(10)*w(26)
endif
! terms of degree 9
if (9.le.mxd) then
 w(363:524) = u(0)*w(201:362)
 w(525:563) = u(1)*w(162:200)
 w(564:589) = u(2)*w(175:200)
 w(590:606) = u(3)*w(91:107)
 w(607:618) = u(4)*w(96:107)
 w(619:626) = u(5)*w(100:107)
 w(627:631) = u(6)*w(103:107)
 w(632:634) = u(7)*w(105:107)
 w(635:637) = u(8)*w(51:53)
 w(638:640) = u(9)*w(51:53)
 w(641:643) = u(10)*w(51:53)
endif
END SUBROUTINE cg7_prib
