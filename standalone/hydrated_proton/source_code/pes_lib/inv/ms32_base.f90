SUBROUTINE ms32_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:ms32_nr-1), v(0:ms32_nsc(mxd)-1)
if (size(x).ne.ms32_nr.or.size(w).ne.ms32_nb(mxd)) then
 stop 'ms32_base: bad dimensions'
endif
call ms32_prims (x, u)
call ms32_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 2 2 2 0 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 1 2 1 0 1 0 0 0
! base,  dnb(0:*): 1 2 6 14 28 52 93 152 242 370
! constant term
w(0) = v(0)
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(3:4) = u(0)*w(1:2)
 w(5) = u(1)*w(2)
 w(6) = u(2)*w(0)
 w(7) = u(3)*w(0)
 w(8) = v(1)
endif
! terms of degree 3
if (3.le.mxd) then
 w(9:14) = u(0)*w(3:8)
 w(15:18) = u(1)*w(5:8)
 w(19) = u(4)*w(0)
 w(20) = u(5)*w(0)
 w(21:22) = v(2:3)
endif
! terms of degree 4
if (4.le.mxd) then
 w(23:36) = u(0)*w(9:22)
 w(37:44) = u(1)*w(15:22)
 w(45:47) = u(2)*w(6:8)
 w(48:49) = u(3)*w(7:8)
 w(50) = v(4)
endif
! terms of degree 5
if (5.le.mxd) then
 w(51:78) = u(0)*w(23:50)
 w(79:92) = u(1)*w(37:50)
 w(93:96) = u(2)*w(19:22)
 w(97:100) = u(3)*w(19:22)
 w(101) = u(4)*w(8)
 w(102) = u(5)*w(8)
endif
! terms of degree 6
if (6.le.mxd) then
 w(103:154) = u(0)*w(51:102)
 w(155:178) = u(1)*w(79:102)
 w(179:184) = u(2)*w(45:50)
 w(185:187) = u(3)*w(48:50)
 w(188:191) = u(4)*w(19:22)
 w(192:194) = u(5)*w(20:22)
 w(195) = v(5)
endif
! terms of degree 7
if (7.le.mxd) then
 w(196:288) = u(0)*w(103:195)
 w(289:329) = u(1)*w(155:195)
 w(330:339) = u(2)*w(93:102)
 w(340:345) = u(3)*w(97:102)
 w(346) = u(4)*w(50)
 w(347) = u(5)*w(50)
endif
! terms of degree 8
if (8.le.mxd) then
 w(348:499) = u(0)*w(196:347)
 w(500:558) = u(1)*w(289:347)
 w(559:575) = u(2)*w(179:195)
 w(576:586) = u(3)*w(185:195)
 w(587:588) = u(4)*w(101:102)
 w(589) = u(5)*w(102)
endif
! terms of degree 9
if (9.le.mxd) then
 w(590:831) = u(0)*w(348:589)
 w(832:921) = u(1)*w(500:589)
 w(922:939) = u(2)*w(330:347)
 w(940:947) = u(3)*w(340:347)
 w(948:955) = u(4)*w(188:195)
 w(956:959) = u(5)*w(192:195)
endif
END SUBROUTINE ms32_base
