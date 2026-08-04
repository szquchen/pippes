SUBROUTINE mg2211_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg2211_nsc(mxd)) then
 stop 'mg2211_secs: bad dimensions'
endif
call mg2211_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:6) = pv(0:5)
endif
if (3.le.mxd) then
 v(7:15) = pv(6:14)
endif
if (4.le.mxd) then
 v(16) = pv(0)*pv(1)
 v(17) = pv(1)*pv(2)
 v(18) = pv(1)*pv(3)
 v(19) = pv(0)*pv(4)
 v(20) = pv(2)*pv(4)
 v(21) = pv(3)*pv(4)
 v(22) = pv(0)*pv(5)
 v(23) = pv(2)*pv(5)
 v(24) = pv(3)*pv(5)
endif
if (5.le.mxd) then
 v(25) = pv(0)*pv(10)
 v(26) = pv(0)*pv(11)
 v(27) = pv(1)*pv(12)
 v(28) = pv(1)*pv(13)
 v(29) = pv(0)*pv(14)
 v(30) = pv(1)*pv(14)
endif
if (6.le.mxd) then
! There are no secondaries at degree 6
endif
if (7.le.mxd) then
 v(31) = pv(0)*pv(1)*pv(14)
endif
! There are no further secondaries
return
END SUBROUTINE mg2211_secs
