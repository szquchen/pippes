SUBROUTINE cg2_base_vec (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:nr-1)
real (kind=wp), intent (out) :: w(0:1+mxd-1)
!-----------------------------------------------------------------------
real (kind=wp) :: u(0:nr-1), v(0:0)
integer :: i
call cg2_prims (x, u)
call cg2_secs (mxd, x, v)
! Trivial code.  Different from other routines in the inv package,
! this one does not have an upper restriction on mxd.  There is only
! one primary and only one secondary.
! constant term
w(0) = v(0)
! higher degrees
do i = 1, mxd
 w(i) = u(0)*w(i-1)
enddo
END SUBROUTINE cg2_base_vec
