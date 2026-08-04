SUBROUTINE mg22_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg22_nsc(mxd)) then
 stop 'mg22_secs: bad dimensions'
endif
call mg22_isecs (mxd, r, pv)
v(0) = 1
if (3.le.mxd) then
 v(1:1) = pv(0:0)
endif
! There are no further secondaries
return
END SUBROUTINE mg22_secs
