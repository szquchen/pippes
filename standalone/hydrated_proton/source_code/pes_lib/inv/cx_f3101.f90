FUNCTION cx_f3101 (nki, r, pc, cf) RESULT (f)
integer, intent (in) :: nki(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:), cf(0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: w(0:size(cf)-1)
call cx_b311 (nki, (/0,1,3/), pc, r, w)
f = dot_product(cf,w)
return
END FUNCTION cx_f3101
