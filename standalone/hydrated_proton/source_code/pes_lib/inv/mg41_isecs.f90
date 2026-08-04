SUBROUTINE mg41_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=4, m2=m*(m-1), mm=m2*(m-2)
integer :: i0, i1, i2, j0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5
if (size(r).ne.nk*nk) then
 stop 'mg41_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5)
pv = 0
j0 = m
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg41_i1 (mxd, m2)
   do i2 = 0, m-1
    if(i2.ne.i0.and.i2.ne.i1) then
     call mg41_ii (mxd, mm)
    endif
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg41_i1 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
endif
if (3.le.mxd) then
 pv(2) = pv(2)+d2(i0,i1)*d(i0,j0)/mu
 pv(4) = pv(4)+d(i0,i1)*d2(i0,j0)/mu
 pv(5) = pv(5)+d(i0,i1)*d(i0,j0)*d(i1,j0)/mu
endif
if (4.le.mxd) then
 pv(6) = pv(6)+d4(i0,i1)/mu
 pv(7) = pv(7)+d3(i0,i1)*d(i0,j0)/mu
 pv(10) = pv(10)+d2(i0,i1)*d2(i0,j0)/mu
 pv(12) = pv(12)+d(i0,i1)*d3(i0,j0)/mu
 pv(13) = pv(13)+d2(i0,i1)*d(i0,j0)*d(i1,j0)/mu
endif
if (5.le.mxd) then
 pv(14) = pv(14)+d5(i0,i1)/mu
 pv(15) = pv(15)+d4(i0,i1)*d(i0,j0)/mu
 pv(17) = pv(17)+d3(i0,i1)*d2(i0,j0)/mu
 pv(19) = pv(19)+d2(i0,i1)*d3(i0,j0)/mu
 pv(20) = pv(20)+d(i0,i1)*d3(i0,j0)*d(i1,j0)/mu
endif
END SUBROUTINE mg41_i1
SUBROUTINE mg41_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(1) = pv(1)+d2(i0,i1)*d(i0,i2)/mu
 pv(3) = pv(3)+d(i0,i1)*d(i0,i2)*d(i0,j0)/mu
endif
if (4.le.mxd) then
 pv(8) = pv(8)+d2(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(9) = pv(9)+d2(i0,i1)*d(i1,i2)*d(i0,j0)/mu
 pv(11) = pv(11)+d(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
endif
if (5.le.mxd) then
 pv(16) = pv(16)+d3(i0,i1)*d(i0,i2)*d(i0,j0)/mu
 pv(18) = pv(18)+d2(i0,i1)*d(i0,i2)*d2(i0,j0)/mu
endif
END SUBROUTINE mg41_ii
END SUBROUTINE mg41_isecs
