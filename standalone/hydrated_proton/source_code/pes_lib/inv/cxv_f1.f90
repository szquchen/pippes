FUNCTION cxv_f1 (nki, cf) RESULT (f)
integer, intent (in) :: nki(0:)
real (kind=wp), intent (in) :: cf(0:)
real (kind=wp) :: f(0:pure_sum(nki)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: w(0:pure_sum(nki)-1,0:size(cf)-1)
call cxv_b1 (nki, (/0/), w)
f = matmul(w,cf)
return
END FUNCTION cxv_f1
