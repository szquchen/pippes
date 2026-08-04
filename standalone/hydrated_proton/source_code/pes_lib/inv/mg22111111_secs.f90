SUBROUTINE mg22111111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg22111111_nsc(mxd)) then
 stop 'mg22111111_secs: bad dimensions'
endif
call mg22111111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:42) = pv(0:41)
endif
if (3.le.mxd) then
 v(43:91) = pv(42:90)
endif
if (4.le.mxd) then
 stop 'mg22111111: degree 4 not implemented'
endif
return
END SUBROUTINE mg22111111_secs
