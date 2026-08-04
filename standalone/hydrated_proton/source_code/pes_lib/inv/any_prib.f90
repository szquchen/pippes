SUBROUTINE any_prib (mxd, npr, npb, u, w)
integer, intent (in) :: mxd, npr(0:), npb(0:)
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:npb(mxd)-1)
!-----------------------------------------------------------------------
integer :: l(0:mxd,0:npr(mxd)), ind, i, k, d, inc
if (size(u).lt.npr(mxd)) then
 stop 'any_prib: bad dimensions'
endif
l(0,0:npr(mxd)) = 0
if (1.le.size(w)) then
 w(0) = 1
endif
ind = 1
do d = 1, mxd
 do k = 1, d
  do i = npr(k-1), npr(k)-1
   l(d,i) = ind
   inc = l(d+1-k,0)-l(d-k,i)
   w(ind:ind+inc-1) = u(i)*w(l(d-k,i):l(d-k,i)+inc-1)
   ind = ind+inc
  enddo
 enddo
 l(d,npr(d):npr(mxd)) = ind
enddo
END SUBROUTINE any_prib
