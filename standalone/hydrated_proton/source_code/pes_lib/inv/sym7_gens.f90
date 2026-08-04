SUBROUTINE sym7_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.sym7_nr) then
 stop 'sym7_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1)
 iord = (/ 1, 0, 2, 3, 4, 5, 6 /)
case (1)
! permutation (0,1,2,3,4,5,6)
 iord = (/ 1, 2, 3, 4, 5, 6, 0 /)
case default
 stop 'sym7_gens: invalid index'
end select
return
END SUBROUTINE sym7_gens
