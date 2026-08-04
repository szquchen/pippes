SUBROUTINE sym2s2_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.sym2s2_nr) then
 stop 'sym2s2_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1)
 iord = (/ 1, 0, 2, 3 /)
case (1)
! permutation (2,3)
 iord = (/ 0, 1, 3, 2 /)
case default
 stop 'sym2s2_gens: invalid index'
end select
return
END SUBROUTINE sym2s2_gens
