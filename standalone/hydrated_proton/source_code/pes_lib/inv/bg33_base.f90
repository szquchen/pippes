SUBROUTINE bg33_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:bg33_nr-1), v(0:bg33_nsc(mxd)-1)
if (size(x).ne.bg33_nr.or.size(w).ne.bg33_nb(mxd)) then
 stop 'bg33_base: bad dimensions'
endif
call bg33_prims (x, u)
call bg33_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 3 3 1 0 1 0 0 0
! secs,  dnsc(0:*): 1 0 0 3 7 9 13 15 20 22
! base,  dnb(0:*): 1 1 4 10 24 51 114 219 424 768
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
 w(12) = u(6)*w(0)
 w(13:15) = v(1:3)
endif
! terms of degree 4
if (4.le.mxd) then
 w(16:25) = u(0)*w(6:15)
 w(26:28) = u(1)*w(3:5)
 w(29:30) = u(2)*w(4:5)
 w(31) = u(3)*w(5)
 w(32) = u(7)*w(0)
 w(33:39) = v(4:10)
endif
! terms of degree 5
if (5.le.mxd) then
 w(40:63) = u(0)*w(16:39)
 w(64:69) = u(1)*w(10:15)
 w(70:75) = u(2)*w(10:15)
 w(76:81) = u(3)*w(10:15)
 w(82:90) = v(11:19)
endif
! terms of degree 6
if (6.le.mxd) then
 w(91:141) = u(0)*w(40:90)
 w(142:155) = u(1)*w(26:39)
 w(156:166) = u(2)*w(29:39)
 w(167:175) = u(3)*w(31:39)
 w(176:181) = u(4)*w(10:15)
 w(182:186) = u(5)*w(11:15)
 w(187:190) = u(6)*w(12:15)
 w(191) = u(8)*w(0)
 w(192:204) = v(20:32)
endif
! terms of degree 7
if (7.le.mxd) then
 w(205:318) = u(0)*w(91:204)
 w(319:345) = u(1)*w(64:90)
 w(346:366) = u(2)*w(70:90)
 w(367:381) = u(3)*w(76:90)
 w(382:389) = u(4)*w(32:39)
 w(390:397) = u(5)*w(32:39)
 w(398:405) = u(6)*w(32:39)
 w(406:408) = u(7)*w(13:15)
 w(409:423) = v(33:47)
endif
! terms of degree 8
if (8.le.mxd) then
 w(424:642) = u(0)*w(205:423)
 w(643:705) = u(1)*w(142:204)
 w(706:754) = u(2)*w(156:204)
 w(755:792) = u(3)*w(167:204)
 w(793:801) = u(4)*w(82:90)
 w(802:810) = u(5)*w(82:90)
 w(811:819) = u(6)*w(82:90)
 w(820:827) = u(7)*w(32:39)
 w(828:847) = v(48:67)
endif
! terms of degree 9
if (9.le.mxd) then
 w(848:1271) = u(0)*w(424:847)
 w(1272:1376) = u(1)*w(319:423)
 w(1377:1454) = u(2)*w(346:423)
 w(1455:1511) = u(3)*w(367:423)
 w(1512:1540) = u(4)*w(176:204)
 w(1541:1563) = u(5)*w(182:204)
 w(1564:1581) = u(6)*w(187:204)
 w(1582:1590) = u(7)*w(82:90)
 w(1591:1593) = u(8)*w(13:15)
 w(1594:1615) = v(68:89)
endif
END SUBROUTINE bg33_base
