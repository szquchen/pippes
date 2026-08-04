SUBROUTINE mgx_setd (r, d, d2, d3, d4, d5, d6, d7, d8, d9)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: d(0:,0:)
real (kind=wp), optional, intent (out) :: d2(0:,0:), d3(0:,0:), &
  d4(0:,0:), d5(0:,0:), d6(0:,0:), d7(0:,0:), d8(0:,0:), d9(0:,0:)
!-----------------------------------------------------------------------
integer :: i, j
if (size(d,1).ne.size(r,1).or.size(d,2).ne.size(r,2).or. &
  size(r,1).ne.size(r,2)) then
 stop 'mgx_setd: bad dimensions'
endif
do j = 0, size(r,2)-1
 do i = 0, j-1
  d(i,j) = (r(i,j)+r(j,i))/2
  d(j,i) = d(i,j)
 enddo
 d(j,j) = 0
enddo
if (present(d2)) then
 d2 = d*d
 if (present(d3)) then
  d3 = d2*d
  if (present(d4)) then
   d4 = d3*d
   if (present(d5)) then
    d5 = d4*d
    if (present(d6)) then
     d6 = d5*d
     if (present(d7)) then
      d7 = d6*d
      if (present(d8)) then
       d8 = d7*d
       if (present(d9)) then
        d9 = d8*d
       endif
      endif
     endif
    endif
   endif
  endif
 endif
endif
return
END SUBROUTINE mgx_setd
