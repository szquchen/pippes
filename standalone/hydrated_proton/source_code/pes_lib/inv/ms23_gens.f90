SUBROUTINE ms23_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.ms23_nr) then
 stop 'ms23_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on all sides
! (00, 10, 01, 11, 02, 12)
! (10, 00, 11, 01, 12, 02)
 iord = (/ 1, 0, 3, 2, 5, 4 /)
case default
 stop 'ms23_gens: invalid index'
end select
return
END SUBROUTINE ms23_gens
