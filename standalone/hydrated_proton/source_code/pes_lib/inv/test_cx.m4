PROGRAM test_cx
use inv, wp=>inv_wp
implicit none
type (cx_t), parameter :: pcv(2:5)= &
  (/ cx_t(9,5,0,8.0), &
  cx_t(9,5,0,8.0), &
  cx_t(8,5,0,8.0), &
  cx_t(8,5,0,8.0) /)
integer, parameter :: nk=NK, nki(0:NKK-1)=(/NKI/)
character (len=*), parameter :: sys='*'
integer :: nb, iord(0:nk-1), ind, i, j
real (kind=wp) :: xn0(0:2,0:nk-1), xn1(0:2,0:nk-1), &
  r0(0:nk-1,0:nk-1), r1(0:nk-1,0:nk-1)
real (kind=wp), allocatable :: w0(:), w1(:)
nb = cx_nbase(nki,sys,pcv)
allocate (w0(0:nb-1), w1(0:nb-1))
call random_number (xn0)
do j = 0, nk-1
 do i = 0, j-1
  r0(i,j) = sqrt(sum((xn0(:,j)-xn0(:,i))**2))
  r0(j,i) = r0(i,j)
 enddo
 r0(j,j) = 0
enddo
call cx_base (nki, sys, pcv, r0, w0)
do ind = 0, mgx_ngen(nki)-1
 call mgx_gens2d (nk, nki, ind, iord)
 r1 = r0(iord,iord)
 call cx_base (nki, sys, pcv, r1, w1)
 write (*,'(i3,f12.8)') ind, sum(abs(w1-w0))
enddo
! Note: the following are not good tests at the present status of the
! inv_mg routines.  It is not required that the basis be strictly
! permuted when we relabel the nuclei, and in several cases (starting
! at nki=(/1,1,3/)) the basis for relabeled nuclei is indeed not a
! permutation of the original one.
! We would have to redesign the inv_mg routines to impose this
! additional symmetry.  Alternatively we should rewrite this testing
! routine to test only that the polynomial space is invariant.
if (2.le.size(nki)) then
 iord = (/ (i,i=nki(0),sum(nki(0:1))-1), (i,i=0,nki(0)-1), &
   (i,i=sum(nki(0:1)),sum(nki)-1) /)
 r1 = r0(iord,iord)
 call cx_base (nki((/1,0,(i,i=2,size(nki)-1)/)), sys, pcv, r1, w1)
 write (*,'(a,f12.8)') 'flip01:', abs(sum(w1)-sum(w0))
endif
if (3.le.size(nki)) then
 iord = (/ (i,i=nki(0),sum(nki)-1), (i,i=0,nki(0)-1) /)
 r1 = r0(iord,iord)
 call cx_base(nki((/(i,i=1,size(nki)-1),0/)), sys, pcv, r1, w1)
 write (*,'(a,f12.8)') 'cycle: ', abs(sum(w1)-sum(w0))
endif
stop
END PROGRAM test_cx
