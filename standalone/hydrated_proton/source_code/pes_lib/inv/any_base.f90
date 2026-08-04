SUBROUTINE any_base (mxd, npr, nsc, nb, u, v, w)
integer, intent (in) :: mxd, npr(0:), nsc(0:), nb(0:)
real (kind=wp), intent (in) :: u(0:), v(0:)
real (kind=wp), intent (out) :: w(0:nb(mxd)-1)
!-----------------------------------------------------------------------
integer :: l(0:mxd,0:npr(mxd)), ind, i, k, d, inc
if (size(u).lt.npr(mxd).or.size(v).lt.nsc(mxd)) then
 stop 'any_base: bad dimensions'
endif
l(0,0:npr(mxd)) = 0
if (1.le.size(w)) then
 w(0) = v(0)
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
 w(ind:ind+nsc(d)-nsc(d-1)-1) = v(nsc(d-1):nsc(d)-1)
 ind = ind+nsc(d)-nsc(d-1)
enddo
END SUBROUTINE any_base
