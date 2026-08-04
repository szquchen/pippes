SUBROUTINE mg11_base_scal (mxd, x0, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x0
real (kind=wp), intent (out) :: w(0:1+mxd-1)
!-----------------------------------------------------------------------
integer :: i
! Trivial code.  Different from other routines in the inv package,
! this one does not have an upper restriction on mxd.
! constant term
w(0) = 1
! higher degrees
do i = 1, mxd
 w(i) = x0*w(i-1)
enddo
END SUBROUTINE mg11_base_scal
