SUBROUTINE pes_dists (xn, d)
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp), intent (out) :: d(0:,0:)
!-----------------------------------------------------------------------
integer :: i, j, n
if (size(d,1).ne.size(xn,2).or.size(d,2).ne.size(xn,2)) then
 stop 'pes_dists: bad dimensions'
endif
n = size(xn,2)
do j = 0, n-1
 do i = 0, j-1
  d(i,j) = sqrt(sum((xn(:,j)-xn(:,i))**2))
  d(j,i) = d(i,j)
 enddo
 d(j,j) = 0
enddo
return
END SUBROUTINE pes_dists
