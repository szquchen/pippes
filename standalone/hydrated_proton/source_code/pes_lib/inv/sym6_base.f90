SUBROUTINE sym6_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:sym6_nr-1), v(0:sym6_nsc(mxd)-1)
if (size(x).ne.sym6_nr.or.size(w).ne.sym6_nb(mxd)) then
 stop 'sym6_base: bad dimensions'
endif
call sym6_prims (x, u)
call sym6_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 1 1 1 1 1 0 0 0
! secs,  dnsc(0:*): 1 0 0 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 1 2 3 5 7 11 14 20 26
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
 w(18) = u(4)*w(0)
endif
! terms of degree 6
if (6.le.mxd) then
 w(19:25) = u(0)*w(12:18)
 w(26:27) = u(1)*w(10:11)
 w(28) = u(2)*w(6)
 w(29) = u(5)*w(0)
endif
! terms of degree 7
if (7.le.mxd) then
 w(30:40) = u(0)*w(19:29)
 w(41:42) = u(1)*w(17:18)
 w(43) = u(2)*w(11)
endif
! terms of degree 8
if (8.le.mxd) then
 w(44:57) = u(0)*w(30:43)
 w(58:61) = u(1)*w(26:29)
 w(62) = u(2)*w(18)
 w(63) = u(3)*w(11)
endif
! terms of degree 9
if (9.le.mxd) then
 w(64:83) = u(0)*w(44:63)
 w(84:86) = u(1)*w(41:43)
 w(87:88) = u(2)*w(28:29)
 w(89) = u(3)*w(18)
endif
END SUBROUTINE sym6_base
