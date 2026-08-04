SUBROUTINE mgx_mkrl2d (nk, x, d)
! Simple revlex order
integer, intent (in) :: nk
real (kind=wp), intent (in) :: x(0:)
real (kind=wp), intent (out) :: d(0:,0:)
!-----------------------------------------------------------------------
integer :: i, j, k
if (size(x).ne.nk*(nk-1)/2.or. &
  size(d,1).ne.nk.or.size(d,2).ne.nk) then
 stop 'mgx_mkrl2d: bad dimensions'
endif
k = 0
do j = 0, nk-1
 do i = 0, j-1
  d(i,j) = x(k)
  d(j,i) = d(i,j)
  k = k+1
 enddo
enddo
do i = 0, nk-1
 d(i,i) = 0
enddo
END SUBROUTINE mgx_mkrl2d
