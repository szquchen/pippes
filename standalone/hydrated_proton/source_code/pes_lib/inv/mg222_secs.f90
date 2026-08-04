SUBROUTINE mg222_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg222_nsc(mxd)) then
 stop 'mg222_secs: bad dimensions'
endif
call mg222_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:3) = pv(0:2)
endif
if (3.le.mxd) then
 v(4:16) = pv(3:15)
endif
if (4.le.mxd) then
 v(17) = pv(0)*pv(1)
 v(18) = pv(0)*pv(2)
 v(19) = pv(1)*pv(2)
 v(20:31) = pv(16:27)
endif
if (5.le.mxd) then
 v(32) = pv(2)*pv(3)
 v(33) = pv(2)*pv(4)
 v(34) = pv(1)*pv(5)
 v(35) = pv(1)*pv(6)
 v(36) = pv(2)*pv(7)
 v(37) = pv(0)*pv(9)
 v(38) = pv(1)*pv(10)
 v(39) = pv(2)*pv(10)
 v(40) = pv(1)*pv(11)
 v(41) = pv(0)*pv(12)
 v(42) = pv(1)*pv(12)
 v(43) = pv(2)*pv(12)
 v(44) = pv(0)*pv(13)
 v(45) = pv(0)*pv(14)
 v(46) = pv(0)*pv(15)
endif
if (6.le.mxd) then
 v(47) = pv(4)*pv(14)
 v(48) = pv(0)*pv(16)
 v(49) = pv(2)*pv(17)
 v(50) = pv(1)*pv(18)
 v(51) = pv(0)*pv(19)
 v(52) = pv(1)*pv(21)
 v(53) = pv(2)*pv(21)
 v(54) = pv(0)*pv(22)
 v(55) = pv(1)*pv(23)
 v(56) = pv(2)*pv(26)
 v(57) = pv(0)*pv(27)
 v(58) = pv(1)*pv(27)
 v(59) = pv(2)*pv(27)
endif
if (7.le.mxd) then
 v(60) = pv(4)*pv(27)
 v(61) = pv(5)*pv(27)
 v(62) = pv(12)*pv(27)
endif
if (8.le.mxd) then
! No secondaries at degree 8
endif
if (9.le.mxd) then
!! There is one secondary at degree 9, but we don't know it.
 stop 'mg222: degree 9 not implemented'
endif
return
END SUBROUTINE mg222_secs
