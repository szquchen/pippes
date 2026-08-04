SUBROUTINE mgx_mk2d (nkj, x, d)
! Block revlex order; block sizes nkj(0:)
integer, intent (in) :: nkj(0:)
real (kind=wp), intent (in) :: x(0:)
real (kind=wp) :: d(0:,0:)
!-----------------------------------------------------------------------
integer :: i, j, k, l0, l1, j0, j1
if (size(x).ne.sum(nkj)*(sum(nkj)-1)/2.or. &
  size(d,1).ne.sum(nkj).or.size(d,2).ne.sum(nkj)) then
 stop 'mgx_mk2d: bad dimensions'
endif
! For reasons of speed we use the code for simple revlex in cases
! where it will give the same result
if (size(nkj).le.1) then
 call mgx_mkrl2d (sum(nkj), x, d)
else if (nkj(1).lt.3.and.all(nkj(2:).lt.2)) then
 call mgx_mkrl2d (sum(nkj), x, d)
else
 k = 0
 j1 = 0
 do l1 = 0, size(nkj)-1
  j0 = 0
  do l0 = 0, l1
   do j = j1, j1+nkj(l1)-1
    do i = j0, min(j-1,j0+nkj(l0)-1)
     d(i,j) = x(k)
     d(j,i) = d(i,j)
     k = k+1
    enddo
   enddo
   j0 = j0+nkj(l0)
  enddo
  j1 = j1+nkj(l1)
 enddo
 do i = 0, sum(nkj)-1
  d(i,i) = 0
 enddo
endif
END SUBROUTINE mgx_mk2d
