SUBROUTINE mg82_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg82_nsc(mxd)) then
 stop 'mg82_secs: bad dimensions'
endif
call mg82_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
endif
if (3.le.mxd) then
endif
if (4.le.mxd) then
endif
if (5.le.mxd) then
endif
if (6.le.mxd) then
 stop 'mg82: degree 6 not implemented'
endif
return
END SUBROUTINE mg82_secs
