FUNCTION cxv_f4111 (nki, r, pc, cf) RESULT (f)
integer, intent (in) :: nki(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:), cf(0:)
real (kind=wp) :: f(0:size(r,1)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: w(0:size(r,1)-1,0:size(cf)-1)
call cxv_b4111 (nki, (/0,1,2,3/), pc, r, w)
f = matmul(w,cf)
return
END FUNCTION cxv_f4111
