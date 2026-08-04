SUBROUTINE sym5_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.sym5_nr) then
 stop 'sym5_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1)
 iord = (/ 1, 0, 2, 3, 4 /)
case (1)
! permutation (0,1,2,3,4)
 iord = (/ 1, 2, 3, 4, 0 /)
case default
 stop 'sym5_gens: invalid index'
end select
return
END SUBROUTINE sym5_gens
