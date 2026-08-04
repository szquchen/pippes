SUBROUTINE t1_base_vec (mxd, x, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp) :: w(0:)
!-----------------------------------------------------------------------
! real (kind=wp) :: u(0:t1_nr-1), v(0:t1_nsc(mxd)-1)
real (kind=wp) :: u(0:t1_nr-1), v(0:0)
integer :: i
if (size(x).ne.t1_nr.or.size(w).ne.1+mxd) then
 stop 't1_base: bad dimensions'
endif
call t1_prims (x, u)
call t1_secs (mxd, x, v)
! Trivial code.  Different from other routines in the inv package,
! this one does not have an upper restriction on mxd.
! constant term
w(0) = v(0)
! higher degrees
do i = 1, mxd
 w(i) = u(0)*w(i-1)
enddo
END SUBROUTINE t1_base_vec
