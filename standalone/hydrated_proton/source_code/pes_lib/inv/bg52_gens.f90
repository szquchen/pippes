SUBROUTINE bg52_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.bg52_nr) then
 stop 'bg52_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the left
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41) =>
! (10, 00, 20, 30, 40, 11, 01, 21, 31, 41)
 iord = (/ 1, 0, 2, 3, 4, 6, 5, 7, 8, 9 /)
case (1)
! permutation (0,1,2,3,4) on the left
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41) =>
! (10, 20, 30, 40, 00, 11, 21, 31, 41, 01)
 iord = (/ 1, 2, 3, 4, 0, 6, 7, 8, 9, 5 /)
case (2)
! permutation (0,1) on the right
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41) =>
! (01, 11, 21, 31, 41, 00, 10, 20, 30, 40)
 iord = (/ 5, 6, 7, 8, 9, 0, 1, 2, 3, 4 /)
case default
 stop 'bg52_gens: invalid index'
end select
return
END SUBROUTINE bg52_gens
