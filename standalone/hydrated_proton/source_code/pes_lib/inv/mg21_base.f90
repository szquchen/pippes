SUBROUTINE mg21_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:mg21_nb(mxd)-1)
!-----------------------------------------------------------------------
integer :: k, l0, m0, l, n, ind
real (kind=wp) :: u(0:nr-1), v(0:mg21_nsc(mxd)-1), &
  w0(0:mg21_npb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mg21_base: bad size r'
!! else if (size(w).ne.mg21_nb(mxd)) then
!!  stop 'mg21_base: bad size w'
endif
call mg21_prims (r, u)
call mg21_prib (mxd, u, w0)
call mg21_secs (mxd, r, v)
! trivial code for this special case
if (size(v).eq.1) then
 w = v(0)*w0
else
 stop 'mg21_base: bad element count'
endif
return
END SUBROUTINE mg21_base
