PROGRAM test_mkprib
use inv, wp=>inv_wp
implicit none
integer, parameter :: mxd=ubound(GRP()_nb,1), nk=GRP()_nk, &
  nr=GRP()_nr, nkj(0:size(GRP()_nkj)-1)=GRP()_nkj, m=GRP()_nb(mxd)
integer :: k
! test the parameters
if (nr.ne.nk*(nk-1)/2.or. &
  sum(nkj).ne.nk) then
 stop 'test - nkj, nk, nr parameter fault'
endif
if (product(factorial(nkj)).ne.GRP()_ngrp) then
 stop 'test - nkj, ngrp parameter fault'
endif
if (sum(min(2,nkj-1)).ne.mgx_ngen(nkj)) then
 stop 'test - nkj, ngen parameter fault'
endif
do k = 0, size(GRP()_nb)-1
 if (GRP()_npr(k).ne.sum(GRP()_dnpr(0:k))) then
  stop 'test - npr parameter fault'
 endif
 if (GRP()_nsc(k).ne.sum(GRP()_dnsc(0:k))) then
  stop 'test - nsc parameter fault'
 endif
 if (GRP()_nb(k).ne.sum(GRP()_dnb(0:k))) then
  stop 'test - nb parameter fault'
 endif
 if (GRP()_npb(k).ne.sum(GRP()_dnpb(0:k))) then
  stop 'test - npb parameter fault'
 endif
enddo
!! write (*,*) 'done test parameters'
! generate the GRP()_prib code
call any_mkprib ('GRP()', mxd, GRP()_dnpr, GRP()_dnpb)
stop
CONTAINS
ELEMENTAL FUNCTION factorial (k) RESULT (n)
integer, intent (in) :: k
integer :: n
integer :: n0, i
n0 = 1
do i = 2, k
 n0 = n0*i
enddo
n = n0
return
END FUNCTION factorial
END PROGRAM test_mkprib
