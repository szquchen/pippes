SUBROUTINE cg2_prib (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:cg2_npb(mxd)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:nr-1)
if (size(x).ne.nr) then
 stop 'cg2_prib: bad size x'
!! else if (size(w).ne.cg2_npb(mxd)) then
!!  stop 'cg2_prib: bad size w'
endif
call cg2_prims (x, u)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 0 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 1 1 1 1 1 1 1 1 1
! empty product
w(0) = 1
! terms of degree 1
if (1.le.mxd) then
 w(1) = u(0)*w(0)
endif
! terms of degree 2
if (2.le.mxd) then
 w(2) = u(0)*w(1)
endif
! terms of degree 3
if (3.le.mxd) then
 w(3) = u(0)*w(2)
endif
! terms of degree 4
if (4.le.mxd) then
 w(4) = u(0)*w(3)
endif
! terms of degree 5
if (5.le.mxd) then
 w(5) = u(0)*w(4)
endif
! terms of degree 6
if (6.le.mxd) then
 w(6) = u(0)*w(5)
endif
! terms of degree 7
if (7.le.mxd) then
 w(7) = u(0)*w(6)
endif
! terms of degree 8
if (8.le.mxd) then
 w(8) = u(0)*w(7)
endif
! terms of degree 9
if (9.le.mxd) then
 w(9) = u(0)*w(8)
endif
END SUBROUTINE cg2_prib
