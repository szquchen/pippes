SUBROUTINE cxv_b1 (nki, ik, w)
integer, intent (in) :: nki(0:), ik(0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: i0
if (any(size(nki).lt.ik)) then
 stop 'cxv_b1: bad nki, ik'
else if (size(ik).ne.mg1_nkk) then
 stop 'cxv_b1: bad dimension ik'
else if (size(w,1).ne.sum(nki)) then
 stop 'cxv_b1: bad dimension w (1)'
else if (size(w,2).ne.1) then
 stop 'cxv_b1: bad dimension w (2)'
endif
w = 0
do i0 = sum(nki(0:ik(0)-1)), sum(nki(0:ik(0)))-1
 w(i0,0) = 1
enddo
return
END SUBROUTINE cxv_b1
