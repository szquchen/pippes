SUBROUTINE cg4_prib (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:cg4_npb(mxd)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:nr-1)
if (size(x).ne.nr) then
 stop 'cg4_prib: bad size x'
!! else if (size(w).ne.cg4_npb(mxd)) then
!!  stop 'cg4_prib: bad size w'
endif
call cg4_prims (x, u)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 2 2 1 0 0 0 0 0
! prib,  dnpb(0:*): 1 1 3 5 9 13 22 30 45 61
! empty product
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(2) = u(0)*w(1)
 w(3) = u(1)*w(0)
 w(4) = u(2)*w(0)
endif
! terms of degree 3
if (3.le.mxd) then
 w(5:7) = u(0)*w(2:4)
 w(8) = u(3)*w(0)
 w(9) = u(4)*w(0)
endif
! terms of degree 4
if (4.le.mxd) then
 w(10:14) = u(0)*w(5:9)
 w(15:16) = u(1)*w(3:4)
 w(17) = u(2)*w(4)
 w(18) = u(5)*w(0)
endif
! terms of degree 5
if (5.le.mxd) then
 w(19:27) = u(0)*w(10:18)
 w(28:29) = u(1)*w(8:9)
 w(30:31) = u(2)*w(8:9)
endif
! terms of degree 6
if (6.le.mxd) then
 w(32:44) = u(0)*w(19:31)
 w(45:48) = u(1)*w(15:18)
 w(49:50) = u(2)*w(17:18)
 w(51:52) = u(3)*w(8:9)
 w(53) = u(4)*w(9)
endif
! terms of degree 7
if (7.le.mxd) then
 w(54:75) = u(0)*w(32:53)
 w(76:79) = u(1)*w(28:31)
 w(80:81) = u(2)*w(30:31)
 w(82) = u(3)*w(18)
 w(83) = u(4)*w(18)
endif
! terms of degree 8
if (8.le.mxd) then
 w(84:113) = u(0)*w(54:83)
 w(114:122) = u(1)*w(45:53)
 w(123:127) = u(2)*w(49:53)
 w(128) = u(5)*w(18)
endif
! terms of degree 9
if (9.le.mxd) then
 w(129:173) = u(0)*w(84:128)
 w(174:181) = u(1)*w(76:83)
 w(182:185) = u(2)*w(80:83)
 w(186:188) = u(3)*w(51:53)
 w(189) = u(4)*w(53)
endif
END SUBROUTINE cg4_prib
