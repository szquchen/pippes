SUBROUTINE mg4_isecs (mxd, r, pv)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: pv(0:)
!-----------------------------------------------------------------------
integer, parameter :: m=nk, m2=m*(m-1), mm=m2*(m-2)
integer :: i0, i1, i2, mu
real (kind=wp), dimension (0:nk-1,0:nk-1) :: d, d2, d3, d4, d5
if (size(r).ne.nk*nk) then
 stop 'mg4_isecs: bad dimensions'
endif
call mgx_setd (r, d, d2, d3, d4, d5)
pv = 0
do i0 = 0, m-1
 do i1 = 0, m-1
  if (i1.ne.i0) then
   call mg4_i1 (mxd, m2)
   do i2 = 0, m-1
    if (i2.ne.i0.and.i2.ne.i1) then
     call mg4_ii (mxd, mm)
    endif
   enddo
  endif
 enddo
enddo
return
CONTAINS
SUBROUTINE mg4_i1 (mxd, mu)
integer, intent (in) :: mxd, mu
if (4.le.mxd) then
 pv(1) = pv(1)+d4(i0,i1)/mu
endif
if (5.le.mxd) then
 pv(2) = pv(2)+d5(i0,i1)/mu
endif
END SUBROUTINE mg4_i1
SUBROUTINE mg4_ii (mxd, mu)
integer, intent (in) :: mxd, mu
if (3.le.mxd) then
 pv(0) = pv(0)+d2(i0,i1)*d(i0,i2)/mu
endif
END SUBROUTINE mg4_ii
END SUBROUTINE mg4_isecs
