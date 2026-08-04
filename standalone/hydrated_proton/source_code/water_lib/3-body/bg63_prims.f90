SUBROUTINE bg63_prims (x, u)
use inv_wp_t
integer, parameter :: m=6, n=3
integer, parameter :: bg63_nr=6*3
real (kind=wp), intent (in) :: x(0:bg63_nr-1)
real (kind=wp), intent (out) :: u(0:bg63_nr-1)
!-----------------------------------------------------------------------
integer :: i, j
real (kind=wp) :: e(0:m-1), f(0:n-1), t0, t1
if (size(x).ne.bg63_nr.or.size(u).ne.bg63_nr) then
 stop 'bg63_prims: bad dimensions'
endif
do i = 0, m-1
 e(i) = sum(x(i:i+m*(n-1):m))/n
enddo
do j = 0, n-1
 f(j) = sum(x(m*j:m*(j+1)-1))/m
enddo
t0 = 0 ; t1 = 0
do j = 0, n-1
 do i = 0, m-1
  t0 = t0+x(i+m*j)**3*f(j)
  t1 = t1+x(i+m*j)**3*e(i)
 enddo
enddo
u(0) = sum(x)/size(x)
u(1) = sum(e**2)/size(e)
u(2) = sum(f**2)/size(f)
u(3) = sum(x**2)/size(x)
u(4) = sum(e**3)/size(e)
u(5) = sum(f**3)/size(f)
u(6) = sum(x**3)/size(x)
u(7) = sum(e**4)/size(e)
u(8) = sum(x**4)/size(x)
u(9) = t0/(m*n)
u(10) = t1/(m*n)
u(11) = sum(e**5)/size(e)
u(12) = sum(x**5)/size(x)
u(13) = sum(e**6)/size(e)
u(14) = sum(x**6)/size(x)
u(15) = sum(x**10)/size(x)
u(16) = sum(x**12)/size(x)
u(17) = sum(x**15)/size(x)
return
END SUBROUTINE bg63_prims
