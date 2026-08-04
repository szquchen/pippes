PROGRAM test_cx_bx
use inv, wp=>inv_wp
implicit none
type (cx_t), parameter :: pc=cx_t(9,5,0,8.0)
integer, parameter :: nkk=size((/NKI/))
integer, parameter :: nki(0:nkk-1)=(/NKI/), nkj(0:nkk-1)=(/NKJ/)
integer :: nk, nb, k
integer, allocatable :: iord(:)
real (kind=wp), allocatable :: r(:,:), x(:), w0(:), w1(:)
nk = sum(nki)
allocate (iord(0:nk-1))
allocate (r(0:nk-1,0:nk-1))
allocate (x(0:nk*(nk-1)/2-1))
call random_number (x)
call mgx_mk2d (nki, x, r)
nb = cx_nbx(nkj,pc%dg)
allocate (w0(0:nb-1), w1(0:nb-1))
call cx_bx (nki, nkj, pc, r, w0)
do k = 0, mgx_ngen(nki)-1
 call mgx_gens2d (nki, k, iord)
 call cx_bx (nki, nkj, pc, r(iord,iord), w1)
 write (*,'(2x,f16.12)') maxval(abs(w1-w0))
enddo
stop
END PROGRAM test_cx_bx
