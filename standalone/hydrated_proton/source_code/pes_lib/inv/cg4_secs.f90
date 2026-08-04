SUBROUTINE cg4_secs (mxd, x, v)
integer, intent (in) :: mxd
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: v(0:)
!-----------------------------------------------------------------------
integer, parameter :: npv=3, nk2=nk*(nk-1), nk3=nk2*(nk-2), &
  nk4=nk3*(nk-3)
integer :: i0, i1, i2, k
real (kind=wp) :: pv(0:npv-1), d(0:nk-1,0:nk-1), d2(0:nk-1,0:nk-1), &
  d3(0:nk-1,0:nk-1), d4(0:nk-1,0:nk-1), d5(0:nk-1,0:nk-1)
if (size(x).ne.nr.or.size(v).ne.cg4_nsc(mxd)) then
 stop 'cg4_secs: bad dimensions'
endif
call cg_setd ()
pv = 0
do i0 = 0, nk-1
 do i1 = 0, nk-1
 if (i1.ne.i0) then
  pv(1) = pv(1)+d4(i0,i1)/nk2
  pv(2) = pv(2)+d5(i0,i1)/nk2
  do i2 = 0, nk-1
  if (i2.ne.i0.and.i2.ne.i1) then
   pv(0) = pv(0)+d2(i0,i1)*d(i0,i2)/nk3
  endif
  enddo
 endif
 enddo
enddo
v(0) = 1
if (3.le.mxd) then
 v(1) = pv(0)
endif
if (4.le.mxd) then
 v(2) = pv(1)
endif
if (5.le.mxd) then
 v(3) = pv(2)
endif
if (6.le.mxd) then
 v(4) = pv(0)*pv(0)
endif
if (9.le.mxd) then
 v(5) = pv(1)*pv(2)
endif
return
CONTAINS
SUBROUTINE cg_setd ()
 integer :: i0, i1, k
 k = 0
 do i1 = 1, nk-1
  do i0 = 0, i1-1
   d(i0,i1) = x(k)
   d(i1,i0) = d(i0,i1)
   d2(i0,i1) = x(k)**2
   d2(i1,i0) = d2(i0,i1)
   d3(i0,i1) = x(k)**3
   d3(i1,i0) = d3(i0,i1)
   d4(i0,i1) = x(k)**4
   d4(i1,i0) = d4(i0,i1)
   d5(i0,i1) = x(k)**5
   d5(i1,i0) = d5(i0,i1)
   k = k+1
  enddo
  d(i1,i1) = 0
  d2(i1,i1) = 0
  d3(i1,i1) = 0
  d4(i1,i1) = 0
  d5(i1,i1) = 0
 enddo
END SUBROUTINE cg_setd
END SUBROUTINE cg4_secs
