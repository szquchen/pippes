SUBROUTINE px_x3y2z1u1_base (xn0, w)
real (kind=wp), intent (in) :: xn0(0:,0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:size(xn0,2)-1,0:size(xn0,2)-1)
if (size(w).ne.px_x3y2z1u1_nbase()) then
 stop 'px_x3y2z1u1_base: bad dimension'
endif
call pes_dists (xn0, r)
call cx_base (pes_x3y2z1u1_nki, pes_x3y2z1u1_sysnew, px_pcv, r, w)
return
END SUBROUTINE px_x3y2z1u1_base
