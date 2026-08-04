SUBROUTINE sym6_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.sym6_nr) then
 stop 'sym6_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1)
 iord = (/ 1, 0, 2, 3, 4, 5 /)
case (1)
! permutation (0,1,2,3,4,5)
 iord = (/ 1, 2, 3, 4, 5, 0 /)
case default
 stop 'sym6_gens: invalid index'
end select
return
END SUBROUTINE sym6_gens
