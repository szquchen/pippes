SUBROUTINE sym2s2_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:sym2s2_nr-1), v(0:sym2s2_nsc(mxd)-1)
if (size(x).ne.sym2s2_nr.or.size(w).ne.sym2s2_nb(mxd)) then
 stop 'sym2s2_base: bad dimensions'
endif
call sym2s2_prims (x, u)
call sym2s2_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 2 2 0 0 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 0 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 2 5 8 12 20 30 40 55 70
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
endif
! terms of degree 3
if (3.le.mxd) then
 w(8:12) = u(0)*w(3:7)
 w(13:15) = u(1)*w(5:7)
endif
! terms of degree 4
if (4.le.mxd) then
 w(16:23) = u(0)*w(8:15)
 w(24:26) = u(1)*w(13:15)
 w(27:28) = u(2)*w(6:7)
 w(29) = u(3)*w(7)
endif
! terms of degree 5
if (5.le.mxd) then
 w(30:43) = u(0)*w(16:29)
 w(44:49) = u(1)*w(24:29)
endif
! terms of degree 6
if (6.le.mxd) then
 w(50:69) = u(0)*w(30:49)
 w(70:75) = u(1)*w(44:49)
 w(76:78) = u(2)*w(27:29)
 w(79) = u(3)*w(29)
endif
! terms of degree 7
if (7.le.mxd) then
 w(80:109) = u(0)*w(50:79)
 w(110:119) = u(1)*w(70:79)
endif
! terms of degree 8
if (8.le.mxd) then
 w(120:159) = u(0)*w(80:119)
 w(160:169) = u(1)*w(110:119)
 w(170:173) = u(2)*w(76:79)
 w(174) = u(3)*w(79)
endif
! terms of degree 9
if (9.le.mxd) then
 w(175:229) = u(0)*w(120:174)
 w(230:244) = u(1)*w(160:174)
endif
END SUBROUTINE sym2s2_base
