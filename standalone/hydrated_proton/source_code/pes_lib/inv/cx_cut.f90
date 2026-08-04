FUNCTION cx_cut (pc, r) result (f)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
integer :: i, j
real (kind=wp) :: d2, d
if (size(r,1).ne.size(r,2).or.size(r,1).lt.2) then
 stop 'cx_cut: bad dimensions'
endif
d2 = 0
do j = 1, size(r,2)-1
 do i = 0, j-1
  d2 = d2+r(i,j)**2+r(j,i)**2
 enddo
enddo
if (d2.eq.0) then
 stop 'cx_cut: zero distance'
endif
d = sqrt(d2/(size(r,2)*(size(r,2)-1)))
if (d/pc%ax.lt.1) then
 f = (1-d/pc%ax)**pc%kx/d**pc%lx
else
 f = 0
endif
return
END FUNCTION cx_cut
