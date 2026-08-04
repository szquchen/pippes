SUBROUTINE mg21111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg21111_nsc(mxd)) then
 stop 'mg21111_secs: bad dimensions'
endif
call mg21111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:6) = pv(0:5)
endif
if (4.le.mxd) then
 v(7) = pv(0)*pv(5)
endif
! There are no further secondaries
return
END SUBROUTINE mg21111_secs
