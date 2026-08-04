subroutine getx0 (nk, r0, x0)
implicit none
integer nk
real r0(0:nk-1,0:nk-1), x0(0:nk*(nk-1)/2-1)
integer i, j, k
k = 0
do j = 0, nk-1
 do i = 0, j-1
  x0(k) = 1.d0*exp(-r0(i,j)/3)
  k = k+1
 enddo
enddo
return
end subroutine getx0
