SUBROUTINE mgx_mkrl1d (nk, d, x)
! Simple revlex order
integer, intent (in) :: nk
real (kind=wp), intent (in) :: d(0:,0:)
real (kind=wp), intent (out) :: x(0:)
!-----------------------------------------------------------------------
integer :: i, j, k
if (size(d,1).ne.nk.or.size(d,2).ne.nk.or. &
  size(x).ne.nk*(nk-1)/2) then
 stop 'mgx_mkrl1d: bad dimensions'
endif
k = 0
do j = 0, nk-1
 do i = 0, j-1
  x(k) = d(i,j)
  k = k+1
 enddo
enddo
END SUBROUTINE mgx_mkrl1d
