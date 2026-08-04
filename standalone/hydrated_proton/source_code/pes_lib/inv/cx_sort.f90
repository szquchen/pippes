PURE FUNCTION cx_sort (ix) RESULT (iord)
! Stable descending sort, O(n^2) method, for small arrays.
integer, intent (in) :: ix(0:)
integer :: iord(0:size(ix)-1)
!-----------------------------------------------------------------------
integer :: i, k, n, ix0(0:size(ix)-1)
n = size(ix)
ix0 = ix
do i = 0, n-1
 k = maxloc(ix0,dim=1)-1
 iord(i) = k
 ix0(k) = -huge(ix0)
enddo
return
END FUNCTION cx_sort
