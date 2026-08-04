SUBROUTINE cxv_b2221 (nki, ik, pc, r, w)
integer, intent (in) :: nki(0:), ik(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: i0, i1, j0, j1, k0, k1, l0
real (kind=wp) :: t0, r0(0:mg2221_nk-1,0:mg2221_nk-1), &
  y0(0:mg2221_nk-1,0:mg2221_nk-1), &
  w0(0:mg2221_nk-1,0:cx_dim(mgv2221_nb,pc%dg)-1), &
  w1(0:size(w,1)-1,0:cx_dim(mgv2221_nb,pc%dg)-1)
if (any(size(nki).lt.ik)) then
 stop 'cxv_b2221: bad nki, ik'
else if (size(ik).ne.mg2221_nkk) then
 stop 'cxv_b2221: bad dimension ik'
else if (size(r,1).ne.sum(nki).or.size(r,2).ne.sum(nki)) then
 stop 'cxv_b2221: bad dimension nki, r'
else if (size(w,1).ne.sum(nki)) then
 stop 'cxv_b2221: bad dimension w (1)'
else if (size(w,2).ne.cx_dim(mgv2221_nb,pc%dg)) then
 stop 'cxv_b2221: bad dimension w (2)'
endif
w1 = 0
if (0.le.pc%dg) then
 do l0 = sum(nki(0:ik(3)-1)), sum(nki(0:ik(3)))-1
  do k1 = sum(nki(0:ik(2)-1))+1, sum(nki(0:ik(2)))-1
   do k0 = sum(nki(0:ik(2)-1)), k1-1
    do j1 = sum(nki(0:ik(1)-1))+1, sum(nki(0:ik(1)))-1
     do j0 = sum(nki(0:ik(1)-1)), j1-1
      do i1 = sum(nki(0:ik(0)-1))+1, sum(nki(0:ik(0)))-1
       do i0 = sum(nki(0:ik(0)-1)), i1-1
        r0 = r((/i0,i1,j0,j1,k0,k1,l0/),(/i0,i1,j0,j1,k0,k1,l0/))
        t0 = cx_cut(pc,r0)
        if (t0.ne.0) then
         call cx_var (pc, r0, y0)
         call mgv2221_base (pc%dg, y0, w0)
         w1((/i0,i1,j0,j1,k0,k1,l0/),:) = &
           w1((/i0,i1,j0,j1,k0,k1,l0/),:)+w0*t0
        endif
       enddo
      enddo
     enddo
    enddo
   enddo
  enddo
 enddo
endif
w = w1
return
END SUBROUTINE cxv_b2221
