SUBROUTINE gi_gmod (mxd, npr, nsc, u, v, w)
integer, intent (in) :: mxd, npr(0:), nsc(0:)
real (kind=wp), intent (in) :: u(0:), v(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
integer :: l(0:mxd,0:npr(mxd)), ind, i, k, d, inc
if (size(u).lt.npr(mxd).or.size(v).lt.nsc(mxd)) then
 stop 'gi_gmod: bad dimensions'
endif
l(0,0:npr(mxd)) = 0
ind = 1
do d = 1, mxd
 do k = 1, d
  do i = npr(k-1), npr(k)-1
   l(d,i) = ind
   inc = l(d+1-k,0)-l(d-k,i)
   ind = ind+inc
  enddo
 enddo
 l(d,npr(d):npr(mxd)) = ind
 ind = ind+nsc(d)-nsc(d-1)
enddo
if (size(w).ne.ind) then
 stop 'gi_gmod: bad size(w)'
endif
w(0) = v(0)
do d = 1, mxd
 do k = 1, d
  do i = npr(k-1), npr(k)-1
   inc = l(d+1-k,0)-l(d-k,i)
   w(l(d,i):l(d,i)+inc-1) = u(i)*w(l(d-k,i):l(d+1-k,0)-1)
  enddo
 enddo
 inc = nsc(d)-nsc(d-1)
 w(l(d,npr(d)):l(d,npr(d))+inc-1) = v(nsc(d-1):nsc(d)-1)
enddo
END SUBROUTINE gi_gmod
