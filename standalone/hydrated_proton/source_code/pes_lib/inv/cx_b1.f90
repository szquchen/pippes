SUBROUTINE cx_b1 (nki, ik, w)
integer, intent (in) :: nki(0:), ik(0:)
real (kind=wp), intent (out) :: w(0:)
!-----------------------------------------------------------------------
if (any(size(nki).lt.ik)) then
 stop 'cx_b1: bad nki, ik'
else if (size(ik).ne.mg1_nkk) then
 stop 'cx_b1: bad dimension ik'
else if (size(w).ne.1) then
 stop 'cx_b1: bad dimension w'
endif
w = (/ nki(ik(0)) /)
END SUBROUTINE cx_b1
