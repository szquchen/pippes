SUBROUTINE mg7_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!! Note: We don't have the secondaries at degrees 8; Magma ran out of
!! memory (4GB max) computing v(273:722).
!-----------------------------------------------------------------------
real (kind=wp) :: pv(0:size(v)-1) !! an upper bound size
if (size(r).ne.nk*nk.or.size(v).ne.mg7_nsc(mxd)) then
 stop 'mg7_secs: bad dimensions'
endif
call mg7_isecs (mxd, r, pv)
v(0) = 1
if (4.le.mxd) then
 v(1:8) = pv(0:7)
endif
if (5.le.mxd) then
 v(9:33) = pv(8:32)
endif
if (6.le.mxd) then
 v(34:103) = pv(33:102)
endif
if (7.le.mxd) then
 v(104:272) = pv(103:271)
endif
if (8.le.mxd) then
 stop 'mg7: degree 8 not implemented'
endif
return
END SUBROUTINE mg7_secs
