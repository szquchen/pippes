SUBROUTINE mg4111111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg4111111_nsc(mxd)) then
 stop 'mg4111111_secs: bad dimensions'
endif
call mg4111111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:21) = pv(0:20)
endif
if (3.le.mxd) then
 v(22:126) = pv(21:125)
endif
if (4.le.mxd) then
 stop 'mg4111111: degree 4 not implemented'
endif
return
END SUBROUTINE mg4111111_secs
