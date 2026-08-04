SUBROUTINE sym4_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.sym4_nr) then
 stop 'sym4_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1)
 iord = (/ 1, 0, 2, 3 /)
case (1)
! permutation (0,1,2,3)
 iord = (/ 1, 2, 3, 0 /)
case default
 stop 'sym4_gens: invalid index'
end select
return
END SUBROUTINE sym4_gens
