subroutine mg42_base (mxd, x, w)
integer, intent (in) :: mxd
real , intent (in) :: x(0:mg42_nr-1)
real , intent (out) :: w(0:mg42_ivb(mxd)-1)
!-----------------------------------------------------------------------
real :: u(0:mg42_nr-1), v(0:mg42_ivs(mxd)-1)
call mg42_prims (x, u)
call mg42_secs (mxd, x, v)
call inv_base (mxd, mg42_ivp, mg42_ivs, mg42_ivb, u, v, w)
end subroutine mg42_base
