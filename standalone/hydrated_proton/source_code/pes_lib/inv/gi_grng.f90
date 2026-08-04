SUBROUTINE gi_grng (mxd, npr, u, w)
! Compute graded ring
integer, intent (in) :: mxd, npr(0:)
real (kind=wp), intent (in) :: u(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
integer :: l(0:mxd,0:npr(mxd)), ind, i, k, d, inc
if (size(u).lt.npr(mxd)) then
 stop 'gi_grng: bad size(u)'
endif
! compute the index ranges
l(0,0:npr(mxd)) = 0
ind = 1
do d = 1, mxd
 do k = 1, d
  do i = npr(k-1), npr(k)-1
   l(d,i) = ind
   ind = ind+l(d+1-k,0)-l(d-k,i)
  enddo
 enddo
 l(d,npr(d):npr(mxd)) = ind
enddo
! test dimensions
if (size(w).ne.ind) then
 stop 'gi_grng: bad size(w)'
endif
! compute the ring
w(0) = 1
do d = 1, mxd
 do k = 1, d
  do i = npr(k-1), npr(k)-1
   inc = l(d+1-k,0)-l(d-k,i)
   w(l(d,i):l(d,i)+inc-1) = u(i)*w(l(d-k,i):l(d-k,i)+inc-1)
  enddo
 enddo
enddo
return
END SUBROUTINE gi_grng
