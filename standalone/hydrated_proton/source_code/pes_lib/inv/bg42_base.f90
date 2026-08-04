SUBROUTINE bg42_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:bg42_nr-1), v(0:bg42_nsc(mxd)-1)
if (size(x).ne.bg42_nr.or.size(w).ne.bg42_nb(mxd)) then
 stop 'bg42_base: bad dimensions'
endif
call bg42_prims (x, u)
call bg42_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 3 1 2 0 1 0 0 0
! secs,  dnsc(0:*): 1 0 0 2 4 4 4 6 6 6
! base,  dnb(0:*): 1 1 4 7 19 32 68 114 210 336
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
 w(60:63) = v(7:10)
endif
! terms of degree 6
if (6.le.mxd) then
 w(64:95) = u(0)*w(32:63)
 w(96:107) = u(1)*w(20:31)
 w(108:116) = u(2)*w(23:31)
 w(117:123) = u(3)*w(25:31)
 w(124:126) = u(4)*w(10:12)
 w(127) = u(7)*w(0)
 w(128:131) = v(11:14)
endif
! terms of degree 7
if (7.le.mxd) then
 w(132:199) = u(0)*w(64:131)
 w(200:212) = u(1)*w(51:63)
 w(213:222) = u(2)*w(54:63)
 w(223:229) = u(3)*w(57:63)
 w(230:235) = u(4)*w(26:31)
 w(236:237) = u(5)*w(11:12)
 w(238:239) = u(6)*w(11:12)
 w(240:245) = v(15:20)
endif
! terms of degree 8
if (8.le.mxd) then
 w(246:359) = u(0)*w(132:245)
 w(360:395) = u(1)*w(96:131)
 w(396:419) = u(2)*w(108:131)
 w(420:434) = u(3)*w(117:131)
 w(435:438) = u(4)*w(60:63)
 w(439:444) = u(5)*w(26:31)
 w(445:449) = u(6)*w(27:31)
 w(450:455) = v(21:26)
endif
! terms of degree 9
if (9.le.mxd) then
 w(456:665) = u(0)*w(246:455)
 w(666:711) = u(1)*w(200:245)
 w(712:744) = u(2)*w(213:245)
 w(745:767) = u(3)*w(223:245)
 w(768:775) = u(4)*w(124:131)
 w(776:779) = u(5)*w(60:63)
 w(780:783) = u(6)*w(60:63)
 w(784:785) = u(7)*w(11:12)
 w(786:791) = v(27:32)
endif
END SUBROUTINE bg42_base
