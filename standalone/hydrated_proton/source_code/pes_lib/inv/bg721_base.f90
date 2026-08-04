SUBROUTINE bg72_base (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:bg72_nr-1), v(0:bg72_nsc(mxd)-1)
if (size(x).ne.bg72_nr.or.size(w).ne.bg72_nb(mxd)) then
 stop 'bg72_base: bad dimensions'
endif
call bg72_prims (x, u)
call bg72_secs (mxd, x, v)
END SUBROUTINE bg72_base
