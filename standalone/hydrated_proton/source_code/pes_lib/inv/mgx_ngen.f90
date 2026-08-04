PURE FUNCTION mgx_ngen (nkj) RESULT (ng)
integer, intent (in) :: nkj(0:)
integer :: ng
!-----------------------------------------------------------------------
integer :: i, n0
n0 = 0
do i = 0, size(nkj)-1
 if (3.le.nkj(i)) then
  n0 = n0+2
 else if (nkj(i).eq.2) then
  n0 = n0+1
 endif
enddo
ng = n0
return
END FUNCTION mgx_ngen
