PURE FUNCTION mgx_id (nkj) RESULT (s)
integer, intent (in) :: nkj(0:)
integer :: s
!-----------------------------------------------------------------------
integer, parameter :: &
  nmax = 12, &
  pt(0:nmax) = (/ 1, 1, 2, 3, 5, 7, 11, 15, 22, 30, 42, 56, 77 /), &
  ps(0:nmax) = (/ 0, 1, 2, 4, 7, 12, 19, 30, 45, 67, 97, 139, 195 /), &
  pp(0:nmax,0:nmax) = reshape((/ &
   1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, &
   0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, &
   0,  1,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2,  2, &
   0,  1,  2,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3, &
   0,  1,  3,  4,  5,  5,  5,  5,  5,  5,  5,  5,  5, &
   0,  1,  3,  5,  6,  7,  7,  7,  7,  7,  7,  7,  7, &
   0,  1,  4,  7,  9, 10, 11, 11, 11, 11, 11, 11, 11, &
   0,  1,  4,  8, 11, 13, 14, 15, 15, 15, 15, 15, 15, &
   0,  1,  5, 10, 15, 18, 20, 21, 22, 22, 22, 22, 22, &
   0,  1,  5, 12, 18, 23, 26, 28, 29, 30, 30, 30, 30, &
   0,  1,  6, 14, 23, 30, 35, 38, 40, 41, 42, 42, 42, &
   0,  1,  6, 16, 27, 37, 44, 49, 52, 54, 55, 56, 56, &
   0,  1,  7, 19, 34, 47, 58, 65, 70, 73, 75, 76, 77/), &
   (/nmax+1,nmax+1/))
integer :: nk, nl, ib, i, j
! Note, pt is the partition function, there "for the record".
! ps are partial sums of the partition function.
! pp(i,j) is the number of partitions of j having length at most i.
nk = sum(nkj)
if (nk.gt.nmax.or.any(nkj.lt.0)) then
 s = -huge(0)
 return
endif
ib = ps(nk)
nl = size(nkj)-count(nkj.eq.0)
i = 1
do while (nk.ne.0)
 ib = ib+pp(nl-1,nk)
 nk = nk-nl
 nl = nl-count(nkj.eq.i)
 i = i+1
enddo
s = ib
return
END FUNCTION mgx_id
