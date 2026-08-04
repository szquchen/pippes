SUBROUTINE mg311_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=3, mm=m*(m-1)
integer :: i0, i1, j0, k0, mu
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2
if (size(r).ne.nk*nk) then
 stop 'mg311_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2)
pv = 0
j0 = m ; k0 = j0+1
do i0 = 0, m-1
 call mg311_i0 (mxd, m)
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg311_ii (mxd, mm)
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg311_i0 (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(2) = pv(2)+d(i0,j0)*d(i0,k0)/mu
endif
if (3.le.mxd) then
 pv(7) = pv(7)+d2(i0,j0)*d(i0,k0)/mu
 pv(9) = pv(9)+d(i0,j0)*d2(i0,k0)/mu
endif
END SUBROUTINE mg311_i0
SUBROUTINE mg311_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,i1)*d(i0,j0)/mu
 pv(1) = pv(1)+d(i0,i1)*d(i0,k0)/mu
endif
if (3.le.mxd) then
 pv(3) = pv(3)+d2(i0,i1)*d(i0,j0)/mu
 pv(4) = pv(4)+d(i0,i1)*d2(i0,j0)/mu
 pv(5) = pv(5)+d2(i0,i1)*d(i0,k0)/mu
 pv(6) = pv(6)+d(i0,i1)*d(i0,j0)*d(i0,k0)/mu
 pv(8) = pv(8)+d(i0,i1)*d2(i0,k0)/mu
endif
END SUBROUTINE mg311_ii
END SUBROUTINE mg311_isecs
