FUNCTION pes2_grad2 (fun, df2dis, xn0) RESULT (g2f0)
! Compute (grad.grad.fun)(xn0)
interface
 function fun (xn0) result (f0)
 use pes0, wp=>pes0_wp
 real (kind=wp), intent (in) :: xn0(0:,0:)
 real (kind=wp) :: f0
 end function fun
end interface
real (kind=wp), intent (in) :: df2dis, xn0(0:,0:)
real (kind=wp) :: g2f0(0:size(xn0)-1,0:size(xn0)-1)
!-----------------------------------------------------------------------
integer :: nd, i2, j2
real (kind=wp) :: fa, fb, fc, fd, xn1(0:size(xn0,1)-1,0:size(xn0,2)-1)
nd = size(xn0,1)
do j2 = 0, size(xn0)-1
 do i2 = 0, j2-1
  xn1 = xn0
  xn1(modulo(i2,nd),i2/nd) = xn1(modulo(i2,nd),i2/nd)-df2dis
  xn1(modulo(j2,nd),j2/nd) = xn1(modulo(j2,nd),j2/nd)-df2dis
  fa = fun(xn1)
  xn1 = xn0
  xn1(modulo(i2,nd),i2/nd) = xn1(modulo(i2,nd),i2/nd)+df2dis
  xn1(modulo(j2,nd),j2/nd) = xn1(modulo(j2,nd),j2/nd)-df2dis
  fb = fun(xn1)
  xn1 = xn0
  xn1(modulo(i2,nd),i2/nd) = xn1(modulo(i2,nd),i2/nd)-df2dis
  xn1(modulo(j2,nd),j2/nd) = xn1(modulo(j2,nd),j2/nd)+df2dis
  fc = fun(xn1)
  xn1 = xn0
  xn1(modulo(i2,nd),i2/nd) = xn1(modulo(i2,nd),i2/nd)+df2dis
  xn1(modulo(j2,nd),j2/nd) = xn1(modulo(j2,nd),j2/nd)+df2dis
  fd = fun(xn1)
  g2f0(i2,j2) = (fa+fd-fb-fc)/(4*df2dis**2)
  g2f0(j2,i2) = g2f0(i2,j2)
 enddo
 xn1 = xn0
 xn1(modulo(j2,nd),j2/nd) = xn1(modulo(j2,nd),j2/nd)-df2dis
 fa = fun(xn1)
 xn1 = xn0
 fb = fun(xn1)
 xn1 = xn0
 xn1(modulo(j2,nd),j2/nd) = xn1(modulo(j2,nd),j2/nd)+df2dis
 fc = fun(xn1)
 g2f0(j2,j2) = (fa+fc-2*fb)/(df2dis**2)
enddo
return
END FUNCTION pes2_grad2
