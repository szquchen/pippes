SUBROUTINE mg32_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=3, n=2, mm=m*(m-1), nn=n
integer :: i0, i1, j0, j1, mu
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5
if (size(r).ne.nk*nk) then
 stop 'mg32_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5)
pv = 0
do j0 = m, m+n-1
 do j1 = m, m+n-1
  if (j1.ne.j0) then
   do i0 = 0, m-1
    call mg32_i0jj (mxd, m*nn)
    do i1 = 0, m-1
     if (i1.ne.i0) then
      call mg32_iijj (mxd, mm*nn)
     endif
    enddo
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg32_i0jj (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(3) = pv(3)+d3(i0,j0)/mu
endif
if (4.le.mxd) then
 pv(9) = pv(9)+d4(i0,j0)/mu
 pv(13) = pv(13)+d3(i0,j0)*d(i0,j1)/mu
endif
if (5.le.mxd) then
 pv(15) = pv(15)+d5(i0,j0)/mu
endif
END SUBROUTINE mg32_i0jj
SUBROUTINE mg32_iijj (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
endif
if (3.le.mxd) then
 pv(1) = pv(1)+d2(i0,i1)*d(i0,j0)/mu
 pv(2) = pv(2)+d(i0,i1)*d2(i0,j0)/mu
 pv(4) = pv(4)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(5) = pv(5)+d2(i0,j0)*d(i1,j0)/mu
 pv(6) = pv(6)+d(i0,i1)*d(i0,j0)*d(i0,j1)/mu
endif
if (4.le.mxd) then
 pv(7) = pv(7)+d2(i0,i1)*d2(i0,j0)/mu
 pv(8) = pv(8)+d(i0,i1)*d3(i0,j0)/mu
 pv(10) = pv(10)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/mu
 pv(11) = pv(11)+d(i0,i1)*d2(i0,j0)*d(i1,j0)/mu
 pv(12) = pv(12)+d3(i0,j0)*d(i1,j0)/mu
endif
if (5.le.mxd) then
 pv(14) = pv(14)+d(i0,i1)*d4(i0,j0)/mu
endif
END SUBROUTINE mg32_iijj
END SUBROUTINE mg32_isecs
