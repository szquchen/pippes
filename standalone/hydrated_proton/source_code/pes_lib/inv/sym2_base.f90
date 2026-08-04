SUBROUTINE sym2_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:sym2_nr-1), v(0:sym2_nsc(mxd)-1)
if (size(x).ne.sym2_nr.or.size(w).ne.sym2_nb(mxd)) then
 stop 'sym2_base: bad dimensions'
endif
call sym2_prims (x, u)
call sym2_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 1 0 0 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 0 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 1 2 2 3 3 4 4 5 5
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
endif
! terms of degree 4
if (4.le.mxd) then
 w(6:7) = u(0)*w(4:5)
 w(8) = u(1)*w(3)
endif
! terms of degree 5
if (5.le.mxd) then
 w(9:11) = u(0)*w(6:8)
endif
! terms of degree 6
if (6.le.mxd) then
 w(12:14) = u(0)*w(9:11)
 w(15) = u(1)*w(8)
endif
! terms of degree 7
if (7.le.mxd) then
 w(16:19) = u(0)*w(12:15)
endif
! terms of degree 8
if (8.le.mxd) then
 w(20:23) = u(0)*w(16:19)
 w(24) = u(1)*w(15)
endif
! terms of degree 9
if (9.le.mxd) then
 w(25:29) = u(0)*w(20:24)
endif
END SUBROUTINE sym2_base
