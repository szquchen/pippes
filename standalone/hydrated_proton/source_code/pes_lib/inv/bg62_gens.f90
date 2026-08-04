SUBROUTINE bg62_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.bg62_nr) then
 stop 'bg62_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the left
! (00, 10, 20, 30, 40, 50, 01, 11, 21, 31, 41, 51) =>
! (10, 00, 20, 30, 40, 50, 11, 01, 21, 31, 41, 51)
 iord = (/ 1, 0, 2, 3, 4, 5, 7, 6, 8, 9, 10, 11 /)
case (1)
! permutation (0,1,2,3,4,5) on the left
! (00, 10, 20, 30, 40, 50, 01, 11, 21, 31, 41, 51) =>
! (10, 20, 30, 40, 50, 00, 11, 21, 31, 41, 51, 01)
 iord = (/ 1, 2, 3, 4, 5, 0, 7, 8, 9, 10, 11, 6 /)
case (2)
! permutation (0,1) on the right
! (00, 10, 20, 30, 40, 50, 01, 11, 21, 31, 41, 51) =>
! (01, 11, 21, 31, 41, 51, 00, 10, 20, 30, 40, 50)
 iord = (/ 6, 7, 8, 9, 10, 11, 0, 1, 2, 3, 4, 5 /)
case default
 stop 'bg62_gens: invalid index'
end select
return
END SUBROUTINE bg62_gens
