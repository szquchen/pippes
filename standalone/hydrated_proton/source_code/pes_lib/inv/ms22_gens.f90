SUBROUTINE ms22_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.ms22_nr) then
 stop 'ms22_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on both sides
! (00, 10, 01, 11) =>
! (10, 00, 11, 01)
 iord = (/ 1, 0, 3, 2 /)
case default
 stop 'ms22_gens: invalid index'
end select
return
END SUBROUTINE ms22_gens
