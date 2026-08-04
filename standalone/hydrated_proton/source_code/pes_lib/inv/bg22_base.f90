SUBROUTINE bg22_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:bg22_nr-1), v(0:bg22_nsc(mxd)-1)
if (size(x).ne.bg22_nr.or.size(w).ne.bg22_nb(mxd)) then
 stop 'bg22_base: bad dimensions'
endif
call bg22_prims (x, u)
call bg22_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 3 0 0 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 0 1 0 0 0 0 0 0
! base,  dnb(0:*): 1 1 4 5 11 14 24 30 45 55
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
 w(10) = v(1)
endif
! terms of degree 4
if (4.le.mxd) then
 w(11:15) = u(0)*w(6:10)
 w(16:18) = u(1)*w(3:5)
 w(19:20) = u(2)*w(4:5)
 w(21) = u(3)*w(5)
endif
! terms of degree 5
if (5.le.mxd) then
 w(22:32) = u(0)*w(11:21)
 w(33) = u(1)*w(10)
 w(34) = u(2)*w(10)
 w(35) = u(3)*w(10)
endif
! terms of degree 6
if (6.le.mxd) then
 w(36:49) = u(0)*w(22:35)
 w(50:55) = u(1)*w(16:21)
 w(56:58) = u(2)*w(19:21)
 w(59) = u(3)*w(21)
endif
! terms of degree 7
if (7.le.mxd) then
 w(60:83) = u(0)*w(36:59)
 w(84:86) = u(1)*w(33:35)
 w(87:88) = u(2)*w(34:35)
 w(89) = u(3)*w(35)
endif
! terms of degree 8
if (8.le.mxd) then
 w(90:119) = u(0)*w(60:89)
 w(120:129) = u(1)*w(50:59)
 w(130:133) = u(2)*w(56:59)
 w(134) = u(3)*w(59)
endif
! terms of degree 9
if (9.le.mxd) then
 w(135:179) = u(0)*w(90:134)
 w(180:185) = u(1)*w(84:89)
 w(186:188) = u(2)*w(87:89)
 w(189) = u(3)*w(89)
endif
END SUBROUTINE bg22_base
