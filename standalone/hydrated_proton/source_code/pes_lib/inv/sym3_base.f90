SUBROUTINE sym3_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:sym3_nr-1), v(0:sym3_nsc(mxd)-1)
if (size(x).ne.sym3_nr.or.size(w).ne.sym3_nb(mxd)) then
 stop 'sym3_base: bad dimensions'
endif
call sym3_prims (x, u)
call sym3_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 1 1 0 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 0 0 0 0 0 0 0 0
! base,  dnb(0:*): 1 1 2 3 4 5 7 8 10 12
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
endif
! terms of degree 5
if (5.le.mxd) then
 w(11:14) = u(0)*w(7:10)
 w(15) = u(1)*w(6)
endif
! terms of degree 6
if (6.le.mxd) then
 w(16:20) = u(0)*w(11:15)
 w(21) = u(1)*w(10)
 w(22) = u(2)*w(6)
endif
! terms of degree 7
if (7.le.mxd) then
 w(23:29) = u(0)*w(16:22)
 w(30) = u(1)*w(15)
endif
! terms of degree 8
if (8.le.mxd) then
 w(31:38) = u(0)*w(23:30)
 w(39:40) = u(1)*w(21:22)
endif
! terms of degree 9
if (9.le.mxd) then
 w(41:50) = u(0)*w(31:40)
 w(51) = u(1)*w(30)
 w(52) = u(2)*w(22)
endif
END SUBROUTINE sym3_base
