SUBROUTINE mgx_gens2d (nkj, ind, iord)
integer, intent (in) :: nkj(0:), ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
integer :: i, k0, n0, k
if (size(iord).ne.sum(nkj)) then
 stop 'mgx_gens2d: bad size iord'
endif
n0 = 0; k0 = 0
do i = 0, size(nkj)-1
 if (nkj(i).lt.2.or. &
   (ind.ne.n0.and.(nkj(i).eq.2.or.ind.ne.n0+1))) then
  iord(k0:k0+nkj(i)-1) = (/ (k0+k, k=0,nkj(i)-1) /)
 else if (ind.eq.n0) then
  iord(k0:k0+nkj(i)-1) = (/ k0+1, k0, (k0+k, k=2,nkj(i)-1) /)
 else if (ind.eq.n0+1) then
  iord(k0:k0+nkj(i)-1) = (/ (k0+modulo(k,nkj(i)), k=1,nkj(i)) /)
 endif
 if (3.le.nkj(i)) then
  n0 = n0+2
 else if (nkj(i).eq.2) then
  n0 = n0+1
 endif
 k0 = k0+nkj(i)
enddo
return
END SUBROUTINE mgx_gens2d
