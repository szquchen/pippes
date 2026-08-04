SUBROUTINE bg33_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.bg33_nr) then
 stop 'bg33_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the left
! (00, 10, 20, 01, 11, 21, 02, 12, 22) =>
! (10, 00, 20, 11, 01, 21, 12, 02, 22)
 iord = (/ 1, 0, 2, 4, 3, 5, 7, 6, 8 /)
case (1)
! permutation (0,1,2) on the left
! (00, 10, 20, 01, 11, 21, 02, 12, 22) =>
! (10, 20, 00, 11, 21, 01, 12, 22, 02)
 iord = (/ 1, 2, 0, 4, 5, 3, 7, 8, 6 /)
case (2)
! permutation (0,1) on the right
! (00, 10, 20, 01, 11, 21, 02, 12, 22) =>
! (01, 11, 21, 00, 10, 20, 02, 12, 22)
 iord = (/ 3, 4, 5, 0, 1, 2, 6, 7, 8 /)
case (3)
! permutation (0,1,2) on the right
! (00, 10, 20, 01, 11, 21, 02, 12, 22) =>
! (01, 11, 21, 02, 12, 22, 00, 10, 20)
 iord = (/ 3, 4, 5, 6, 7, 8, 0, 1, 2 /)
case default
 stop 'bg33_gens: invalid index'
end select
return
END SUBROUTINE bg33_gens
