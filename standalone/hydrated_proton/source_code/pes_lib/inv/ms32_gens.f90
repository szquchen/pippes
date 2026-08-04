SUBROUTINE ms32_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.ms32_nr) then
 stop 'ms32_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on both sides
! (00, 10, 20, 01, 11, 21) =>
! (10, 00, 20, 11, 01, 21)
 iord = (/ 1, 0, 2, 4, 3, 5 /)
case (1)
! permutation (0,1,2) on both sides
! (00, 10, 20, 01, 11, 21) =>
! (10, 20, 00, 11, 21, 01)
 iord = (/ 1, 2, 0, 4, 5, 3 /)
case default
 stop 'ms32_gens: invalid index'
end select
return
END SUBROUTINE ms32_gens
