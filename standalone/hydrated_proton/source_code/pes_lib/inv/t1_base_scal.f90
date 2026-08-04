SUBROUTINE t1_base_scal (mxd, x0, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x0
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
integer :: i
if (size(w).ne.1+mxd) then
 stop 't1_base: bad dimensions'
endif
! Trivial code.  Different from other routines in the inv package,
! this one does not have an upper restriction on mxd.
! constant term
w(0) = 1
! higher degrees
do i = 1, mxd
 w(i) = x0*w(i-1)
enddo
END SUBROUTINE t1_base_scal
