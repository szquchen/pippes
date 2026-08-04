SUBROUTINE cg4_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:cg4_nb(mxd)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:nr-1), v(0:cg4_nsc(mxd)-1)
if (size(x).ne.nr) then
 stop 'cg4_base: bad size x'
!! else if (size(w).ne.cg4_nb(mxd)) then
!!  stop 'cg4_base: bad size w'
endif
call cg4_prims (x, u)
call cg4_secs (mxd, x, v)
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 2 2 1 0 0 0 0 0
! secs,  dnsc(0:*): 1 0 0 1 1 1 1 0 0 1
! base,  dnb(0:*): 1 1 3 6 11 18 32 48 75 111
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
endif
! terms of degree 3
if (3.le.mxd) then
 w(5:7) = u(0)*w(2:4)
 w(8) = u(3)*w(0)
 w(9) = u(4)*w(0)
 w(10) = v(1)
endif
! terms of degree 4
if (4.le.mxd) then
 w(11:16) = u(0)*w(5:10)
 w(17:18) = u(1)*w(3:4)
 w(19) = u(2)*w(4)
 w(20) = u(5)*w(0)
 w(21) = v(2)
endif
! terms of degree 5
if (5.le.mxd) then
 w(22:32) = u(0)*w(11:21)
 w(33:35) = u(1)*w(8:10)
 w(36:38) = u(2)*w(8:10)
 w(39) = v(3)
endif
! terms of degree 6
if (6.le.mxd) then
 w(40:57) = u(0)*w(22:39)
 w(58:62) = u(1)*w(17:21)
 w(63:65) = u(2)*w(19:21)
 w(66:68) = u(3)*w(8:10)
 w(69:70) = u(4)*w(9:10)
 w(71) = v(4)
endif
! terms of degree 7
if (7.le.mxd) then
 w(72:103) = u(0)*w(40:71)
 w(104:110) = u(1)*w(33:39)
 w(111:114) = u(2)*w(36:39)
 w(115:116) = u(3)*w(20:21)
 w(117:118) = u(4)*w(20:21)
 w(119) = u(5)*w(10)
endif
! terms of degree 8
if (8.le.mxd) then
 w(120:167) = u(0)*w(72:119)
 w(168:181) = u(1)*w(58:71)
 w(182:190) = u(2)*w(63:71)
 w(191) = u(3)*w(39)
 w(192) = u(4)*w(39)
 w(193:194) = u(5)*w(20:21)
endif
! terms of degree 9
if (9.le.mxd) then
 w(195:269) = u(0)*w(120:194)
 w(270:285) = u(1)*w(104:119)
 w(286:294) = u(2)*w(111:119)
 w(295:300) = u(3)*w(66:71)
 w(301:303) = u(4)*w(69:71)
 w(304) = u(5)*w(39)
 w(305) = v(5)
endif
END SUBROUTINE cg4_base
