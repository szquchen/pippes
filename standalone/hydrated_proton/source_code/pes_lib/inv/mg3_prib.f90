SUBROUTINE mg3_prib (mxd, u, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:mg3_npb(mxd)-1)
!-----------------------------------------------------------------------
if (size(u).ne.nr) then
 stop 'mg3_prib: bad size u'
!! else if (size(w).ne.mg3_npb(mxd)) then
!!  stop 'mg3_prib: bad size w'
endif
! The following code was obtained using these parameters
! prims, dnpr(0:*): 0 1 1 1 0 0 0 0 0 0 &
!   0 0 0 0 0 0 0 0 0 0
! prib,  dnpb(0:*): 1 1 2 3 4 5 7 8 10 12 &
!   14 16 19 21 24 27 30 33 37 40
! constant term
w(0) = 1
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
! terms of degree 10
if (10.le.mxd) then
 w(53:64) = u(0)*w(41:52)
 w(65:66) = u(1)*w(39:40)
endif
! terms of degree 11
if (11.le.mxd) then
 w(67:80) = u(0)*w(53:66)
 w(81:82) = u(1)*w(51:52)
endif
! terms of degree 12
if (12.le.mxd) then
 w(83:98) = u(0)*w(67:82)
 w(99:100) = u(1)*w(65:66)
 w(101) = u(2)*w(52)
endif
! terms of degree 13
if (13.le.mxd) then
 w(102:120) = u(0)*w(83:101)
 w(121:122) = u(1)*w(81:82)
endif
! terms of degree 14
if (14.le.mxd) then
 w(123:143) = u(0)*w(102:122)
 w(144:146) = u(1)*w(99:101)
endif
! terms of degree 15
if (15.le.mxd) then
 w(147:170) = u(0)*w(123:146)
 w(171:172) = u(1)*w(121:122)
 w(173) = u(2)*w(101)
endif
! terms of degree 16
if (16.le.mxd) then
 w(174:200) = u(0)*w(147:173)
 w(201:203) = u(1)*w(144:146)
endif
! terms of degree 17
if (17.le.mxd) then
 w(204:233) = u(0)*w(174:203)
 w(234:236) = u(1)*w(171:173)
endif
! terms of degree 18
if (18.le.mxd) then
 w(237:269) = u(0)*w(204:236)
 w(270:272) = u(1)*w(201:203)
 w(273) = u(2)*w(173)
endif
! terms of degree 19
if (19.le.mxd) then
 w(274:310) = u(0)*w(237:273)
 w(311:313) = u(1)*w(234:236)
endif
END SUBROUTINE mg3_prib
