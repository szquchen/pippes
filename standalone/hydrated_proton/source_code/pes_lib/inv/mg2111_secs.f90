SUBROUTINE mg2111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg2111_nsc(mxd)) then
 stop 'mg2111_secs: bad dimensions'
endif
call mg2111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:3) = pv(0:2)
endif
! There are no further secondaries
return
END SUBROUTINE mg2111_secs
