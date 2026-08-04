subroutine mg42_prims (x, u)
real , intent (in) :: x(0:mg42_nr-1)
real , intent (out) :: u(0:mg42_nr-1)
!-----------------------------------------------------------------------
integer, parameter :: m=4, n=2, m2=m*(m-1)/2, n2=n*(n-1)/2
integer :: i, j
real  :: x0(0:m2-1), x1(0:m*n-1), x2(0:n2-1), t0(0:m-1), &
  e(0:m-1), f(0:n-1)
x0 = x(0:m2-1)
x1 = x(m2:m2+m*n-1)
x2 = x(m2+m*n:m2+m*n+n2-1)
t0(0) = (x0(0)+x0(1)+x0(3))/3
t0(1) = (x0(0)+x0(2)+x0(4))/3
t0(2) = (x0(1)+x0(2)+x0(5))/3
t0(3) = (x0(3)+x0(4)+x0(5))/3
do i = 0, m-1
 e(i) = sum(x1(i:i+m*(n-1):m))/n
enddo
do j = 0, n-1
 f(j) = sum(x1(m*j:m*(j+1)-1))/m
enddo
u(0) = sum(x0)/size(x0)
u(1) = sum(x1)/size(x1)
u(2) = sum(x2)/size(x2)
u(3) = sum(t0**2)/size(t0)
u(4) = sum(x0**2)/size(x0)
u(5) = sum(e**2)/size(e)
u(6) = sum(f**2)/size(f)
u(7) = sum(x1**2)/size(x1)
u(8) = sum(t0**3)/size(t0)
u(9) = sum(x0**3)/size(x0)
u(10) = sum(e**3)/size(e)
u(11) = sum(t0**4)/size(t0)
u(12) = sum(e**4)/size(e)
u(13) = sum(x1**4)/size(x1)
u(14) = sum(x1**6)/size(x1)
return
end subroutine mg42_prims
