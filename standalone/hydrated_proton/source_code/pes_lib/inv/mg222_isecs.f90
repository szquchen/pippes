SUBROUTINE mg222_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, n=2, l=2, mm=m, nn=n, ll=l
integer :: i0, i1, j0, j1, k0, k1
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3
if (size(r).ne.nk*nk) then
 stop 'mg222_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3)
pv = 0
do k0 = m+n, m+n+l-1
 do k1 = m+n, m+n+l-1
  if (k1.ne.k0) then
   do j0 = m, m+n-1
    do j1 = m, m+n-1
     if (j1.ne.j0) then
      do i0 = 0, m-1
       do i1 = 0, m-1
        if (i1.ne.i0) then
         call mg222_all (mxd, mm*nn*ll)
        endif
       enddo
      enddo
     endif
    enddo
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg222_all (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,j0)*d(i0,k0)/mu
 pv(1) = pv(1)+d(i0,j0)*d(j0,k0)/mu
 pv(2) = pv(2)+d(i0,k0)*d(j0,k0)/mu
endif
if (3.le.mxd) then
 pv(3) = pv(3)+d3(i0,j0)/mu
 pv(4) = pv(4)+d2(i0,j0)*d(i0,k0)/mu
 pv(5) = pv(5)+d(i0,j0)*d2(i0,k0)/mu
 pv(6) = pv(6)+d3(i0,k0)/mu
 pv(7) = pv(7)+d2(i0,j0)*d(j0,k0)/mu
 pv(8) = pv(8)+d(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(9) = pv(9)+d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(10) = pv(10)+d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(11) = pv(11)+d2(i0,k0)*d(j0,k0)/mu
 pv(12) = pv(12)+d(i0,j0)*d(i0,k1)*d(j0,k0)/mu
 pv(13) = pv(13)+d(i0,j0)*d2(j0,k0)/mu
 pv(14) = pv(14)+d(i0,k0)*d2(j0,k0)/mu
 pv(15) = pv(15)+d3(j0,k0)/mu
endif
if (4.le.mxd) then
 pv(16) = pv(16)+d2(i0,j0)*d2(i0,k0)/mu
 pv(17) = pv(17)+d2(i0,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(18) = pv(18)+d(i0,j0)*d(i1,j0)*d(i0,k0)*d(j0,k0)/mu
 pv(19) = pv(19)+d(i0,j0)*d(i0,j1)*d(i0,k0)*d(j0,k0)/mu
 pv(20) = pv(20)+d(i0,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(21) = pv(21)+d(i1,j0)*d2(i0,k0)*d(j0,k0)/mu
 pv(22) = pv(22)+d(i0,j0)*d(i0,k0)*d(i0,k1)*d(j0,k0)/mu
 pv(23) = pv(23)+d2(i0,j0)*d2(j0,k0)/mu
 pv(24) = pv(24)+d(i0,j0)*d(i0,k0)*d2(j0,k0)/mu
 pv(25) = pv(25)+d(i0,j1)*d(i0,k0)*d2(j0,k0)/mu
 pv(26) = pv(26)+d2(i0,k0)*d2(j0,k0)/mu
 pv(27) = pv(27)+d(i0,j0)*d(i0,k1)*d2(j0,k0)/mu
endif
END SUBROUTINE mg222_all
END SUBROUTINE mg222_isecs
