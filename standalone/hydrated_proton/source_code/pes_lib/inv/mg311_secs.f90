SUBROUTINE mg311_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg311_nsc(mxd)) then
 stop 'mg311_secs: bad dimensions'
endif
call mg311_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:3) = pv(0:2)
endif
if (3.le.mxd) then
 v(4:10) = pv(3:9)
endif
if (4.le.mxd) then
 v(11) = pv(0)*pv(0)
 v(12) = pv(0)*pv(1)
 v(13) = pv(1)*pv(1)
 v(14) = pv(0)*pv(2)
 v(15) = pv(1)*pv(2)
 v(16) = pv(2)*pv(2)
endif
if (5.le.mxd) then
 v(17) = pv(1)*pv(4)
 v(18) = pv(0)*pv(5)
 v(19) = pv(0)*pv(7)
 v(20) = pv(0)*pv(8)
 v(21) = pv(0)*pv(9)
 v(22) = pv(1)*pv(9)
endif
if (6.le.mxd) then
 v(23) = pv(3)*pv(4)
 v(24) = pv(3)*pv(6)
 v(25) = pv(3)*pv(7)
 v(26) = pv(3)*pv(8)
 v(27) = pv(5)*pv(8)
 v(28) = pv(3)*pv(9)
 v(29) = pv(4)*pv(9)
 v(30) = pv(5)*pv(9)
 v(31) = pv(6)*pv(9)
 v(32) = pv(7)*pv(9)
endif
if (7.le.mxd) then
 v(33) = pv(0)*pv(0)*pv(8)
 v(34) = pv(0)*pv(0)*pv(9)
 v(35) = pv(0)*pv(1)*pv(9)
endif
! There are no further secondaries
return
END SUBROUTINE mg311_secs
