SUBROUTINE mg2111111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg2111111_nsc(mxd)) then
 stop 'mg2111111_secs: bad dimensions'
endif
call mg2111111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:15) = pv(0:14)
endif
if (3.le.mxd) then
! there are no secondaries at degree 3
endif
if (4.le.mxd) then
 v(16) = pv(0)*pv(5)
 v(17) = pv(0)*pv(8)
 v(18) = pv(0)*pv(9)
 v(19) = pv(1)*pv(9)
 v(20) = pv(2)*pv(9)
 v(21) = pv(0)*pv(12)
 v(22) = pv(0)*pv(13)
 v(23) = pv(1)*pv(13)
 v(24) = pv(2)*pv(13)
 v(25) = pv(0)*pv(14)
 v(26) = pv(1)*pv(14)
 v(27) = pv(2)*pv(14)
 v(28) = pv(3)*pv(14)
 v(29) = pv(4)*pv(14)
 v(30) = pv(5)*pv(14)
endif
if (5.le.mxd) then
! No secondaries at this degree
endif
if (6.le.mxd) then
 stop 'mg2111111: degree 6 not implemented'
endif
return
END SUBROUTINE mg2111111_secs
