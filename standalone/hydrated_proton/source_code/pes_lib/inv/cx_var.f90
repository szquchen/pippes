SUBROUTINE cx_var (pc, r, y)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: y(0:,0:)
!-----------------------------------------------------------------------
integer :: i, j
if (size(r,1).ne.size(r,2).or.size(y,1).ne.size(r,1).or. &
  size(y,2).ne.size(r,2)) then
 stop 'cx_var: bad dimensions'
endif
do j = 0, size(r,2)-1
 do i = 0, size(r,1)-1
  if (i.ne.j) then
   if (pc%bx.eq.0.0_wp) then
    y(i,j) = r(i,j)
   else if (pc%bx.gt.0) then
!   exponential transformation
    y(i,j) = exp(-r(i,j)/pc%bx)
   else if (pc%bx.lt.0) then
!   linear at small r, tends to abs(pc%bx) at large r
    y(i,j) = r(i,j)/sqrt(1+(r(i,j)/pc%bx)**2)
   endif
  else
   y(i,j) = 0
  endif
 enddo
enddo
return
END SUBROUTINE cx_var
