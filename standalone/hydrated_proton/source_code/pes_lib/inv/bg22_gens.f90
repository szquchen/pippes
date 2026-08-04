SUBROUTINE bg22_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.bg22_nr) then
 stop 'bg22_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the left
! (00, 10, 01, 11) =>
! (10, 00, 11, 01)
 iord = (/ 1, 0, 3, 2 /)
case (1)
! permutation (0,1) on the right
! (00, 10, 01, 11) =>
! (01, 11, 00, 10)
 iord = (/ 2, 3, 0, 1 /)
case default
 stop 'bg22_gens: invalid index'
end select
return
END SUBROUTINE bg22_gens
