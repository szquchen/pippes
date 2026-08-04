SUBROUTINE mg11_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:mg11_nb(mxd)-1)
!-----------------------------------------------------------------------
integer :: k, l0, m0, l, n, ind
real (kind=wp) :: u(0:nr-1), v(0:mg11_nsc(mxd)-1), &
  w0(0:mg11_npb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mg11_base: bad size r'
!! else if (size(w).ne.mg11_nb(mxd)) then
!!  stop 'mg11_base: bad size w'
endif
call mg11_prims (r, u)
call mg11_prib (mxd, u, w0)
call mg11_secs (mxd, r, v)
! trivial code for this special case
if (size(v).eq.1) then
 w = v(0)*w0
else
 stop 'mg11_base: bad element count'
endif
return
END SUBROUTINE mg11_base
