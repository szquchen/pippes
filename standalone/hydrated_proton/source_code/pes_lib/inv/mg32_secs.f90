SUBROUTINE mg32_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg32_nsc(mxd)) then
 stop 'mg32_secs: bad dimensions'
endif
call mg32_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:1) = pv(0:0)
endif
if (3.le.mxd) then
 v(2:7) = pv(1:6)
endif
if (4.le.mxd) then
 v(8) = pv(0)*pv(0)
 v(9:15) = pv(7:13)
endif
if (5.le.mxd) then
 v(16) = pv(0)*pv(3)
 v(17) = pv(0)*pv(4)
 v(18) = pv(0)*pv(5)
 v(19) = pv(0)*pv(6)
 v(20:21) = pv(14:15)
endif
if (6.le.mxd) then
 v(22) = pv(5)*pv(5)
 v(23) = pv(1)*pv(6)
 v(24) = pv(2)*pv(6)
 v(25) = pv(3)*pv(6)
 v(26) = pv(4)*pv(6)
 v(27) = pv(5)*pv(6)
 v(28) = pv(6)*pv(6)
 v(29) = pv(0)*pv(7)
 v(30) = pv(0)*pv(9)
 v(31) = pv(0)*pv(10)
 v(32) = pv(0)*pv(12)
 v(33) = pv(0)*pv(13)
endif
if (7.le.mxd) then
 v(34) = pv(2)*pv(10)
 v(35) = pv(6)*pv(10)
 v(36) = pv(1)*pv(11)
 v(37) = pv(4)*pv(11)
 v(38) = pv(6)*pv(11)
 v(39) = pv(1)*pv(12)
 v(40) = pv(5)*pv(12)
 v(41) = pv(6)*pv(12)
 v(42) = pv(1)*pv(13)
 v(43) = pv(4)*pv(13)
 v(44) = pv(5)*pv(13)
 v(45) = pv(6)*pv(13)
 v(46) = pv(0)*pv(14)
 v(47) = pv(0)*pv(15)
endif
if (8.le.mxd) then
 v(48) = pv(10)*pv(13)
 v(49) = pv(1)*pv(14)
 v(50) = pv(4)*pv(14)
 v(51) = pv(6)*pv(14)
 v(52) = pv(1)*pv(15)
 v(53) = pv(2)*pv(15)
 v(54) = pv(3)*pv(15)
 v(55) = pv(4)*pv(15)
 v(56) = pv(6)*pv(15)
endif
if (9.le.mxd) then
 v(57) = pv(0)*pv(0)*pv(14)
 v(58) = pv(8)*pv(14)
 v(59) = pv(10)*pv(14)
 v(60) = pv(13)*pv(14)
 v(61) = pv(7)*pv(15)
 v(62) = pv(8)*pv(15)
 v(63) = pv(10)*pv(15)
 v(64) = pv(12)*pv(15)
endif
return
END SUBROUTINE mg32_secs
