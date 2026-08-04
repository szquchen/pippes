SUBROUTINE ms32_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: n=3, r=2
integer :: i, j
if (size(x).ne.ms32_nr.or.size(u).ne.ms32_nr) then
 stop 'ms32_prims: bad dimensions'
endif
do i = 0, n-1
 do j = 0, r-1
  u(i*r+j) = sum(x(j*n:(j+1)*n-1)**(i+1))/n
 enddo
enddo
return
END SUBROUTINE ms32_prims
