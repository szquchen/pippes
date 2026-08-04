SUBROUTINE cg6_prims (x, u)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: u(0:)
!-----------------------------------------------------------------------
integer :: i0, i1, i2, k
real (kind=wp) :: d(0:nk-1,0:nk-1), t1(0:nk-1)
if (size(x).ne.nr.or.size(u).ne.nr) then
 stop 'cg6_prims: bad dimensions'
endif
! Recall revlex order,
! x(0:14) = (/ d01, d02, d12, d03, d13, d23, d04, d14, d24, d34, d05, ... /)
k = 0
do i1 = 0, nk-1
 do i0 = 0, i1-1
  d(i0,i1) = x(k)
  d(i1,i0) = x(k)
  k = k+1
 enddo
 d(i1,i1) = 0
enddo
t1(0) = (x(0)+x(1)+x(3)+x(6)+x(10))/5
t1(1) = (x(0)+x(2)+x(4)+x(7)+x(11))/5
t1(2) = (x(1)+x(2)+x(5)+x(8)+x(12))/5
t1(3) = (x(3)+x(4)+x(5)+x(9)+x(13))/5
t1(4) = (x(6)+x(7)+x(8)+x(9)+x(14))/5
t1(5) = (x(10)+x(11)+x(12)+x(13)+x(14))/5
u = 0
u(0) = sum(x)/size(x)
u(1) = sum(t1**2)/size(t1)
u(2) = sum(x**2)/size(x)
u(3) = sum(t1**3)/size(t1)
u(4) = sum(x**3)/size(x)
u(7) = sum(t1**4)/size(t1)
u(8) = sum(x**4)/size(x)
u(10) = sum(t1**5)/size(t1)
u(11) = sum(x**5)/size(x)
u(13) = sum(t1**6)/size(t1)
u(14) = sum(x**6)/size(x)
do i0 = 0, nk-1
 do i1 = 0, nk-1
 if (i1.ne.i0) then
  do i2 = 0, nk-1
  if (i2.ne.i0.and.i2.ne.i1) then
   u(5) = u(5)+d(i0,i1)**2*d(i0,i2)/120
   u(6) = u(6)+d(i0,i1)*d(i0,i2)*d(i1,i2)/120
   u(9) = u(9)+d(i0,i1)**3*d(i0,i2)/120
   u(12) = u(12)+d(i0,i1)**4*d(i0,i2)/120
  endif
  enddo
 endif
 enddo
enddo
return
END SUBROUTINE cg6_prims
