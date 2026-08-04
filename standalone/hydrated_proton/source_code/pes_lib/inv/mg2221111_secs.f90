SUBROUTINE mg2221111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg2221111_nsc(mxd)) then
 stop 'mg2221111_secs: bad dimensions'
endif
call mg2221111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:45) = pv(0:44)
endif
if (3.le.mxd) then
 v(46:154) = pv(45:153)
endif
if (4.le.mxd) then
 stop 'mg2221111: degree 4 not implemented'
endif
return
END SUBROUTINE mg2221111_secs
