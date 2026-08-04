FUNCTION pes2_grad (fun, dfdis, xn0, acc) RESULT (gf0)
! Compute (grad.fun)(xn0)
interface
 function fun (xn0) result (f0)
 use pes0, wp=>pes0_wp
 real (kind=wp), intent (in) :: xn0(0:,0:)
 real (kind=wp) :: f0
 end function fun
end interface
real (kind=wp), intent (in) :: dfdis, xn0(0:,0:)
integer, intent (in), optional :: acc
real (kind=wp) :: gf0(0:size(xn0,1)-1,0:size(xn0,2)-1)
!-----------------------------------------------------------------------
integer :: iacc, i, j
real (kind=wp) :: fa, fb, fc, fd, xn1(0:size(xn0,1)-1,0:size(xn0,2)-1)
if (present(acc)) then
 iacc = acc
else
 iacc = 0
endif
do j = 0, size(xn0,2)-1
 do i = 0, size(xn0,1)-1
  xn1 = xn0
  xn1(i,j) = xn0(i,j)-dfdis
  fa = fun(xn1)
  xn1(i,j) = xn0(i,j)+dfdis
  fb = fun(xn1)
  if (iacc.eq.0) then
   gf0(i,j) = (fb-fa)/(2*dfdis)
  else
   xn1(i,j) = xn0(i,j)-2*dfdis
   fc = fun(xn1)
   xn1(i,j) = xn0(i,j)+2*dfdis
   fd = fun(xn1)
   gf0(i,j) = (8*(fb-fa)-(fd-fc))/(12*dfdis)
  endif
 enddo
enddo
return
END FUNCTION pes2_grad
