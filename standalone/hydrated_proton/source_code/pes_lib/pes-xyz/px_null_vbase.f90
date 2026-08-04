SUBROUTINE px_null_vbase (xn0, w)
real (kind=wp), intent (in) :: xn0(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
if (size(w,1).ne.size(xn0,2)) then
 stop 'px_null_vbase: bad dimension w (1)'
else if (size(w,2).ne.px_null_nvbase()) then
 stop 'px_null_vbase: bad dimension w (2)'
endif
w = 0
return
END SUBROUTINE px_null_vbase
