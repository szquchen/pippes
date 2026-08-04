SUBROUTINE sym2t2_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:sym2t2_nr-1), v(0:sym2t2_nsc(mxd)-1)
if (size(x).ne.sym2t2_nr.or.size(w).ne.sym2t2_nb(mxd)) then
 stop 'sym2t2_base: bad dimensions'
endif
call sym2t2_prims (x, u)
call sym2t2_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 3 1 0 0 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 0 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 3 7 13 22 34 50 70 95 125
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
 w(10) = u(3)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(11:17) = u(0)*w(4:10)
 w(18:21) = u(1)*w(7:10)
 w(22:23) = u(2)*w(9:10)
endif
! terms of degree 4
if (4.le.mxd) then
 w(24:36) = u(0)*w(11:23)
 w(37:42) = u(1)*w(18:23)
 w(43:44) = u(2)*w(22:23)
 w(45) = u(3)*w(10)
endif
! terms of degree 5
if (5.le.mxd) then
 w(46:67) = u(0)*w(24:45)
 w(68:76) = u(1)*w(37:45)
 w(77:79) = u(2)*w(43:45)
endif
! terms of degree 6
if (6.le.mxd) then
 w(80:113) = u(0)*w(46:79)
 w(114:125) = u(1)*w(68:79)
 w(126:128) = u(2)*w(77:79)
 w(129) = u(3)*w(45)
endif
! terms of degree 7
if (7.le.mxd) then
 w(130:179) = u(0)*w(80:129)
 w(180:195) = u(1)*w(114:129)
 w(196:199) = u(2)*w(126:129)
endif
! terms of degree 8
if (8.le.mxd) then
 w(200:269) = u(0)*w(130:199)
 w(270:289) = u(1)*w(180:199)
 w(290:293) = u(2)*w(196:199)
 w(294) = u(3)*w(129)
endif
! terms of degree 9
if (9.le.mxd) then
 w(295:389) = u(0)*w(200:294)
 w(390:414) = u(1)*w(270:294)
 w(415:419) = u(2)*w(290:294)
endif
END SUBROUTINE sym2t2_base
