SUBROUTINE cxv_b2 (nki, ik, pc, r, w)
integer, intent (in) :: nki(0:), ik(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: i0, i1
real (kind=wp) :: t0, r0(0:mg2_nk-1,0:mg2_nk-1), &
  y0(0:mg2_nk-1,0:mg2_nk-1), &
  w0(0:mg2_nk-1,0:cx_dim(mgv2_nb,pc%dg)-1), &
  w1(0:size(w,1)-1,0:cx_dim(mgv2_nb,pc%dg)-1)
w1 = 0
if (0.le.pc%dg) then
 do i1 = sum(nki(0:ik(0)-1))+1, sum(nki(0:ik(0)))-1
  do i0 = sum(nki(0:ik(0)-1)), i1-1
   r0 = r((/i0,i1/),(/i0,i1/))
   t0 = cx_cut(pc,r0)
   if (t0.ne.0) then
    call cx_var (pc, r0, y0)
    call mgv2_base (pc%dg, y0, w0)
    w1((/i0,i1/),:) = w1((/i0,i1/),:)+w0*t0
   endif
  enddo
 enddo
endif
w = w1
return
END SUBROUTINE cxv_b2
