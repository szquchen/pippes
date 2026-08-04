SUBROUTINE bg32_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:bg32_nr-1), v(0:bg32_nsc(mxd)-1)
if (size(x).ne.bg32_nr.or.size(w).ne.bg32_nb(mxd)) then
 stop 'bg32_base: bad dimensions'
endif
call bg32_prims (x, u)
call bg32_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 3 1 0 0 1 0 0 0
! secs,  dnsc(0:*): 1 0 0 2 3 1 1 2 1 1
! base,  dnb(0:*): 1 1 4 7 16 26 50 76 126 185
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
 w(26:28) = v(3:5)
endif
! terms of degree 5
if (5.le.mxd) then
 w(29:44) = u(0)*w(13:28)
 w(45:47) = u(1)*w(10:12)
 w(48:50) = u(2)*w(10:12)
 w(51:53) = u(3)*w(10:12)
 w(54) = v(6)
endif
! terms of degree 6
if (6.le.mxd) then
 w(55:80) = u(0)*w(29:54)
 w(81:89) = u(1)*w(20:28)
 w(90:95) = u(2)*w(23:28)
 w(96:99) = u(3)*w(25:28)
 w(100:102) = u(4)*w(10:12)
 w(103) = u(5)*w(0)
 w(104) = v(7)
endif
! terms of degree 7
if (7.le.mxd) then
 w(105:154) = u(0)*w(55:104)
 w(155:164) = u(1)*w(45:54)
 w(165:171) = u(2)*w(48:54)
 w(172:175) = u(3)*w(51:54)
 w(176:178) = u(4)*w(26:28)
 w(179:180) = v(8:9)
endif
! terms of degree 8
if (8.le.mxd) then
 w(181:256) = u(0)*w(105:180)
 w(257:280) = u(1)*w(81:104)
 w(281:295) = u(2)*w(90:104)
 w(296:304) = u(3)*w(96:104)
 w(305) = u(4)*w(54)
 w(306) = v(10)
endif
! terms of degree 9
if (9.le.mxd) then
 w(307:432) = u(0)*w(181:306)
 w(433:458) = u(1)*w(155:180)
 w(459:474) = u(2)*w(165:180)
 w(475:483) = u(3)*w(172:180)
 w(484:488) = u(4)*w(100:104)
 w(489:490) = u(5)*w(11:12)
 w(491) = v(11)
endif
END SUBROUTINE bg32_base
