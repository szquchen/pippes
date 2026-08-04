SUBROUTINE mg442_base (mxd, r, w)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:mg442_nb(mxd)-1)
!-----------------------------------------------------------------------
integer :: k, l0, m0, l, n, ind
real (kind=wp) :: u(0:nr-1), v(0:mg442_nsc(mxd)-1), &
  w0(0:mg442_npb(mxd)-1)
if (size(r,1).ne.nk.or.size(r,2).ne.nk) then
 stop 'mg442_base: bad size r'
!! else if (size(w).ne.mg442_nb(mxd)) then
!!  stop 'mg442_base: bad size w'
endif
call mg442_prims (r, u)
call mg442_prib (mxd, u, w0)
call mg442_secs (mxd, r, v)
ind = 0
do n = 0, mxd
 do k = 0, n
  if (k.eq.0) then
   l0 = 0
  else
   l0 = mg442_nsc(k-1)
  endif
  if (k.eq.n) then
   m0 = 0
  else
   m0 = mg442_npb(n-k-1)
  endif
  do l = l0, mg442_nsc(k)-1
   w(ind:ind+mg442_dnpb(n-k)-1) = v(l)*w0(m0:mg442_npb(n-k)-1)
   ind = ind+mg442_dnpb(n-k)
  enddo
 enddo
enddo
if (ind.ne.mg442_nb(mxd)) then
 stop 'mg442_base: bad element count'
endif
return
END SUBROUTINE mg442_base
