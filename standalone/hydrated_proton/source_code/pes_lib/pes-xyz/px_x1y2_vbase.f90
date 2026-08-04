SUBROUTINE px_x1y2_vbase (xn0, w)
real (kind=wp), intent (in) :: xn0(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:size(xn0,2)-1,0:size(xn0,2)-1)
if (size(w,1).ne.size(xn0,2)) then
 stop 'px_x1y2_vbase: bad dimension w (1)'
else if (size(w,2).ne.px_x1y2_nvbase()) then
 stop 'px_x1y2_vbase: bad dimension w (2)'
endif
call pes_dists (xn0, r)
call cxv_base (pes_x1y2_nki, pes_x1y2_sysnew, px_vpcv, r, w)
return
END SUBROUTINE px_x1y2_vbase
