SUBROUTINE mg211111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg211111_nsc(mxd)) then
 stop 'mg211111_secs: bad dimensions'
endif
call mg211111_isecs (mxd, r, pv)
v(0) = 1
if (2.le.mxd) then
 v(1:10) = pv(0:9)
endif
if (4.le.mxd) then
 v(11) = pv(0)*pv(5)
 v(12) = pv(0)*pv(8)
 v(13) = pv(0)*pv(9)
 v(14) = pv(1)*pv(9)
 v(15) = pv(2)*pv(9)
endif
! There are no further secondaries
return
END SUBROUTINE mg211111_secs
