SUBROUTINE mg21111111_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=2, mm=m
integer :: i0, i1, j0, k0, l0, m0, n0, o0, p0
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d
if (size(r).ne.nk*nk) then
 stop 'mg21111111_isecs: bad dimensions'
endif
call mgx_setd (r, d)
pv = 0
j0 = m ; k0 = j0+1 ; l0 = k0+1 ; m0 = l0+1
n0 = m0+1 ; o0 = n0+1 ; p0 = o0+1
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg21111111_all (mxd, mm)
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg21111111_all (mxd, mu)
integer, intent (in) :: mxd, mu
if (2.le.mxd) then
 pv(0) = pv(0)+d(i0,j0)*d(i0,k0)/mu
 pv(1) = pv(1)+d(i0,j0)*d(i0,l0)/mu
 pv(2) = pv(2)+d(i0,k0)*d(i0,l0)/mu
 pv(3) = pv(3)+d(i0,j0)*d(i0,m0)/mu
 pv(4) = pv(4)+d(i0,k0)*d(i0,m0)/mu
 pv(5) = pv(5)+d(i0,l0)*d(i0,m0)/mu
 pv(6) = pv(6)+d(i0,j0)*d(i0,n0)/mu
 pv(7) = pv(7)+d(i0,k0)*d(i0,n0)/mu
 pv(8) = pv(8)+d(i0,l0)*d(i0,n0)/mu
 pv(9) = pv(9)+d(i0,m0)*d(i0,n0)/mu
 pv(10) = pv(10)+d(i0,j0)*d(i0,o0)/mu
 pv(11) = pv(11)+d(i0,k0)*d(i0,o0)/mu
 pv(12) = pv(12)+d(i0,l0)*d(i0,o0)/mu
 pv(13) = pv(13)+d(i0,m0)*d(i0,o0)/mu
 pv(14) = pv(14)+d(i0,n0)*d(i0,o0)/mu
 pv(15) = pv(15)+d(i0,j0)*d(i0,p0)/mu
 pv(16) = pv(16)+d(i0,k0)*d(i0,p0)/mu
 pv(17) = pv(17)+d(i0,l0)*d(i0,p0)/mu
 pv(18) = pv(18)+d(i0,m0)*d(i0,p0)/mu
 pv(19) = pv(19)+d(i0,n0)*d(i0,p0)/mu
 pv(20) = pv(20)+d(i0,o0)*d(i0,p0)/mu
endif
END SUBROUTINE mg21111111_all
END SUBROUTINE mg21111111_isecs
