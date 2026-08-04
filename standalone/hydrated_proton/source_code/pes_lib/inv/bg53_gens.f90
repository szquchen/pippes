SUBROUTINE bg53_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.bg53_nr) then
 stop 'bg53_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the left
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41, 02, 12, 22, 32, 42) =>
! (10, 00, 20, 30, 40, 11, 01, 21, 31, 41, 12, 02, 22, 32, 42)
 iord = (/ 1, 0, 2, 3, 4, 6, 5, 7, 8, 9, 11, 10, 12, 13, 14 /)
case (1)
! permutation (0,1,2,3) on the left
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41, 02, 12, 22, 32, 42) =>
! (10, 20, 30, 40, 00, 11, 21, 31, 41, 01, 12, 22, 32, 42, 02)
 iord = (/ 1, 2, 3, 4, 0, 6, 7, 8, 9, 5, 11, 12, 13, 14, 10 /)
case (2)
! permutation (0,1) on the right
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41, 02, 12, 22, 32, 42) =>
! (01, 11, 21, 31, 41, 00, 10, 20, 30, 40, 02, 12, 22, 32, 42)
 iord = (/ 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 10, 11, 12, 13, 14 /)
case (3)
! permutation (0,1,2) on the right
! (00, 10, 20, 30, 40, 01, 11, 21, 31, 41, 02, 12, 22, 32, 42) =>
! (01, 11, 21, 31, 41, 02, 12, 22, 32, 42, 00, 10, 20, 30, 40)
 iord = (/ 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 0, 1, 2, 3, 4 /)
case default
 stop 'bg53_gens: invalid index'
end select
return
END SUBROUTINE bg53_gens
