SUBROUTINE t3_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:t3_nr-1), v(0:t3_nsc(mxd)-1)
if (size(x).ne.t3_nr.or.size(w).ne.t3_nb(mxd)) then
 stop 't3_base: bad dimensions'
endif
call t3_prims (x, u)
call t3_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 3 0 0 0 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 0 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 3 6 10 15 21 28 36 45 55
! constant term
w(0) = v(0)
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
 w(2) = u(1)*w(0)
 w(3) = u(2)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(4:6) = u(0)*w(1:3)
 w(7:8) = u(1)*w(2:3)
 w(9) = u(2)*w(3)
endif
! terms of degree 3
if (3.le.mxd) then
 w(10:15) = u(0)*w(4:9)
 w(16:18) = u(1)*w(7:9)
 w(19) = u(2)*w(9)
endif
! terms of degree 4
if (4.le.mxd) then
 w(20:29) = u(0)*w(10:19)
 w(30:33) = u(1)*w(16:19)
 w(34) = u(2)*w(19)
endif
! terms of degree 5
if (5.le.mxd) then
 w(35:49) = u(0)*w(20:34)
 w(50:54) = u(1)*w(30:34)
 w(55) = u(2)*w(34)
endif
! terms of degree 6
if (6.le.mxd) then
 w(56:76) = u(0)*w(35:55)
 w(77:82) = u(1)*w(50:55)
 w(83) = u(2)*w(55)
endif
! terms of degree 7
if (7.le.mxd) then
 w(84:111) = u(0)*w(56:83)
 w(112:118) = u(1)*w(77:83)
 w(119) = u(2)*w(83)
endif
! terms of degree 8
if (8.le.mxd) then
 w(120:155) = u(0)*w(84:119)
 w(156:163) = u(1)*w(112:119)
 w(164) = u(2)*w(119)
endif
! terms of degree 9
if (9.le.mxd) then
 w(165:209) = u(0)*w(120:164)
 w(210:218) = u(1)*w(156:164)
 w(219) = u(2)*w(164)
endif
END SUBROUTINE t3_base
