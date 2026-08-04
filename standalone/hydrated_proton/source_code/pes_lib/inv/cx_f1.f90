FUNCTION cx_f1 (nki, cf) RESULT (f)
integer, intent (in) :: nki(0:)
real (kind=wp), intent (in) :: cf(0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: w(0:size(cf)-1)
call cx_b1 (nki, (/0/), w)
f = dot_product(cf,w)
return
END FUNCTION cx_f1
