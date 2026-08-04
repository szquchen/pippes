SUBROUTINE bg22_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, n=2
integer :: i, j
real (kind=wp) :: e(0:m-1), f(0:n-1)
if (size(x).ne.bg22_nr.or.size(u).ne.bg22_nr) then
 stop 'bg22_prims: bad dimensions'
endif
do i = 0, m-1
 e(i) = sum(x(i:i+m*(n-1):m))/n
enddo
do j = 0, n-1
 f(j) = sum(x(m*j:m*(j+1)-1))/m
enddo
u(0) = sum(x)/size(x)
u(1) = sum(e**2)/size(e)
u(2) = sum(f**2)/size(f)
u(3) = sum(x**2)/size(x)
return
END SUBROUTINE bg22_prims
