SUBROUTINE bg43_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.bg43_nr) then
 stop 'bg43_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the left
! (00, 10, 20, 30, 01, 11, 21, 31, 02, 12, 22, 32) =>
! (10, 00, 20, 30, 11, 01, 21, 31, 12, 02, 22, 32)
 iord = (/ 1, 0, 2, 3, 5, 4, 6, 7, 9, 8, 10, 11 /)
case (1)
! permutation (0,1,2,3) on the left
! (00, 10, 20, 30, 01, 11, 21, 31, 02, 12, 22, 32) =>
! (10, 20, 30, 00, 11, 21, 31, 01, 12, 22, 32, 02)
 iord = (/ 1, 2, 3, 0, 5, 6, 7, 4, 9, 10, 11, 8 /)
case (2)
! permutation (0,1) on the right
! (00, 10, 20, 30, 01, 11, 21, 31, 02, 12, 22, 32) =>
! (01, 11, 21, 31, 00, 10, 20, 30, 02, 12, 22, 32)
 iord = (/ 4, 5, 6, 7, 0, 1, 2, 3, 8, 9, 10, 11 /)
case (3)
! permutation (0,1,2) on the right
! (00, 10, 20, 30, 01, 11, 21, 31, 02, 12, 22, 32) =>
! (01, 11, 21, 31, 02, 12, 22, 32, 00, 10, 20, 30)
 iord = (/ 4, 5, 6, 7, 8, 9, 10, 11, 0, 1, 2, 3 /)
case default
 stop 'bg43_gens: invalid index'
end select
return
END SUBROUTINE bg43_gens
