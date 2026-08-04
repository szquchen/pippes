SUBROUTINE mg221_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg221_nsc(mxd)) then
 stop 'mg221_secs: bad dimensions'
endif
call mg221_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:2) = pv(0:1)
endif
if (3.le.mxd) then
 v(3:6) = pv(2:5)
endif
if (4.le.mxd) then
 v(7) = pv(0)*pv(1)
endif
! There are no further secondaries
return
END SUBROUTINE mg221_secs
