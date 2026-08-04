SUBROUTINE sym3_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.sym3_nr) then
 stop 'sym3_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1)
 iord = (/ 1, 0, 2 /)
case (1)
! permutation (0,1,2)
 iord = (/ 1, 2, 0 /)
case default
 stop 'sym3_gens: invalid index'
end select
return
END SUBROUTINE sym3_gens
