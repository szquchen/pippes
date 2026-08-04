SUBROUTINE mg11_base_vec (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:1+mxd-1)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:nr-1), v(0:0)
integer :: i
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mg11_base: bad size r'
endif
call mg11_prims (r, u)
call mg11_secs (mxd, r, v)
! Trivial code.  Different from other routines in the inv package,
! this one does not have an upper restriction on mxd.  There is only
! one primary and only one secondary.
! constant term
w(0) = v(0)
! higher degrees
do i = 1, mxd
 w(i) = u(0)*w(i-1)
enddo
END SUBROUTINE mg11_base_vec
