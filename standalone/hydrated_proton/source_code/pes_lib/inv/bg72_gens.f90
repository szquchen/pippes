SUBROUTINE bg72_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.bg72_nr) then
 stop 'bg72_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the left
! (00, 10, 20, 30, 40, 50, 60, 01, 11, 21, 31, 41, 51, 61) =>
! (10, 00, 20, 30, 40, 50, 60, 11, 01, 21, 31, 41, 51, 61)
 iord = (/ 1, 0, 2, 3, 4, 5, 6, 8, 7, 9, 10, 11, 12, 13 /)
case (1)
! permutation (0,1,2,3,4,5,6) on the left
! (00, 10, 20, 30, 40, 50, 60, 01, 11, 21, 31, 41, 51, 61) =>
! (10, 20, 30, 40, 50, 60, 00, 11, 21, 31, 41, 51, 61, 01)
 iord = (/ 1, 2, 3, 4, 5, 6, 0, 8, 9, 10, 11, 12, 13, 7 /)
case (2)
! permutation (0,1) on the right
! (00, 10, 20, 30, 40, 50, 60, 01, 11, 21, 31, 41, 51, 61) =>
! (01, 11, 21, 31, 41, 51, 61, 00, 10, 20, 30, 40, 50, 60)
 iord = (/ 7, 8, 9, 10, 11, 12, 13, 0, 1, 2, 3, 4, 5, 6 /)
case default
 stop 'bg72_gens: invalid index'
end select
return
END SUBROUTINE bg72_gens
