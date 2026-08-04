SUBROUTINE sym4_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:sym4_nr-1), v(0:sym4_nsc(mxd)-1)
if (size(x).ne.sym4_nr.or.size(w).ne.sym4_nb(mxd)) then
 stop 'sym4_base: bad dimensions'
endif
call sym4_prims (x, u)
call sym4_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 1 1 1 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 0 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 1 2 3 5 6 9 11 15 18
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
endif
! terms of degree 3
if (3.le.mxd) then
 w(4:5) = u(0)*w(2:3)
 w(6) = u(2)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(7:9) = u(0)*w(4:6)
 w(10) = u(1)*w(3)
 w(11) = u(3)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(12:16) = u(0)*w(7:11)
 w(17) = u(1)*w(6)
endif
! terms of degree 6
if (6.le.mxd) then
 w(18:23) = u(0)*w(12:17)
 w(24:25) = u(1)*w(10:11)
 w(26) = u(2)*w(6)
endif
! terms of degree 7
if (7.le.mxd) then
 w(27:35) = u(0)*w(18:26)
 w(36) = u(1)*w(17)
 w(37) = u(2)*w(11)
endif
! terms of degree 8
if (8.le.mxd) then
 w(38:48) = u(0)*w(27:37)
 w(49:51) = u(1)*w(24:26)
 w(52) = u(3)*w(11)
endif
! terms of degree 9
if (9.le.mxd) then
 w(53:67) = u(0)*w(38:52)
 w(68:69) = u(1)*w(36:37)
 w(70) = u(2)*w(26)
endif
END SUBROUTINE sym4_base
