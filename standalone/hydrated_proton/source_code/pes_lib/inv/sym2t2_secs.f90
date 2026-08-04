SUBROUTINE sym2t2_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
if (size(x).ne.sym2t2_nr.or.size(v).ne.sym2t2_nsc(mxd)) then
 stop 'sym2t2_secs: bad dimensions'
endif
! There is only the trivial secondary
v(0) = 1
return
END SUBROUTINE sym2t2_secs
