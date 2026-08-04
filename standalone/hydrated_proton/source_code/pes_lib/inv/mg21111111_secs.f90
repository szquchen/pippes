SUBROUTINE mg21111111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg21111111_nsc(mxd)) then
 stop 'mg21111111_secs: bad dimensions'
endif
call mg21111111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:21) = pv(0:20)
endif
if (3.le.mxd) then
! there are no secondaries at degree 3
endif
if (4.le.mxd) then
 v(22) = pv(0)*pv(5)
 v(23) = pv(0)*pv(8)
 v(24) = pv(0)*pv(9)
 v(25) = pv(1)*pv(9)
 v(26) = pv(2)*pv(9)
 v(27) = pv(0)*pv(12)
 v(28) = pv(0)*pv(13)
 v(29) = pv(1)*pv(13)
 v(30) = pv(2)*pv(13)
 v(31) = pv(0)*pv(14)
 v(32) = pv(1)*pv(14)
 v(33) = pv(2)*pv(14)
 v(34) = pv(3)*pv(14)
 v(35) = pv(4)*pv(14)
 v(36) = pv(5)*pv(14)
 v(37) = pv(0)*pv(17)
 v(38) = pv(0)*pv(18)
 v(39) = pv(1)*pv(18)
 v(40) = pv(2)*pv(18)
 v(41) = pv(0)*pv(19)
 v(42) = pv(1)*pv(19)
 v(43) = pv(2)*pv(19)
 v(44) = pv(3)*pv(19)
 v(45) = pv(4)*pv(19)
 v(46) = pv(5)*pv(19)
 v(47) = pv(0)*pv(20)
 v(48) = pv(1)*pv(20)
 v(49) = pv(2)*pv(20)
 v(50) = pv(3)*pv(20)
 v(51) = pv(4)*pv(20)
 v(52) = pv(5)*pv(20)
 v(53) = pv(6)*pv(20)
 v(54) = pv(7)*pv(20)
 v(55) = pv(8)*pv(20)
 v(56) = pv(9)*pv(20)
endif
if (5.le.mxd) then
! There are no new secondaries at degree 5
endif
if (6.le.mxd) then
 stop 'mg21111111: degree 6 not implemented'
endif
return
END SUBROUTINE mg21111111_secs
