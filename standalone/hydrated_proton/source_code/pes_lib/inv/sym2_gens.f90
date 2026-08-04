SUBROUTINE sym2_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.sym2_nr) then
 stop 'sym2_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1)
 iord = (/ 1, 0 /)
case default
 stop 'sym2_gens: invalid index'
end select
return
END SUBROUTINE sym2_gens
