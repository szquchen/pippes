subroutine getvec (ms, mr, xn, vec)
use inv_mg42_t
implicit none
! version for X4Y2
integer nk, ms, mr
parameter (nk=6)
real xn(0:2,0:nk-1), vec(0:ms+3*mr-1)
integer k, l, deg
real rvec(0:3), x0(0:nk-1,0:nk-1), r0(0:nk-1,0:nk-1)
!-----------------------------------------------------------------------
deg = -1
do k = 9, 0, -1
 if (ms.eq.mg42_ivb(k)) then
  deg = k
 endif
enddo
if (deg.eq.-1) then
 stop 'getvec: bad ms'
endif
vec = 0
call getr0 (nk, xn, r0)
call getx0 (nk, r0, x0)
call mg42_base (deg, x0, vec(0:ms-1))
call getrvec (4, r0, rvec)
do l = 0, mr-1
 do k = 0, 2
  vec(ms+3*l+k) = rvec(k+1)*vec(l)
 enddo
enddo
return
end subroutine getvec
