SUBROUTINE mg11111_secs (mxd, r, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
if (size(r).ne.nk*nk.or.size(v).ne.mg11111_nsc(mxd)) then
 stop 'mg11111_secs: bad dimensions'
endif
! There is only the trivial secondary
v(0) = 1
return
END SUBROUTINE mg11111_secs
