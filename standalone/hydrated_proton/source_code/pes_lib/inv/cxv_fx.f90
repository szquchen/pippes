FUNCTION cxv_fx (nki, nkj, r, pc, cf) RESULT (f)
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:), cf(0:)
real (kind=wp) :: f(0:size(r,1)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: w(0:size(r,1)-1,0:size(cf)-1)
call cxv_bx (nki, nkj, pc, r, w)
f = matmul(w,cf)
return
END FUNCTION cxv_fx
