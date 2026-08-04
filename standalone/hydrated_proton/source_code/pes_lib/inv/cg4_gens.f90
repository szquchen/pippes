SUBROUTINE cg4_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.nr) then
 stop 'cg4_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the vertices
! (01, 02, 12, 03, 13, 23) =>
! (01, 12, 02, 13, 03, 23)
 iord = (/ 0, 2, 1, 4, 3, 5 /)
case (1)
! permutation (0,1,2,3) on the vertices
! (01, 02, 12, 03, 13, 23) =>
! (12, 13, 23, 01, 02, 03)
 iord = (/ 2, 4, 5, 0, 1, 3 /)
case default
 stop 'cg4_gens: invalid index'
end select
return
END SUBROUTINE cg4_gens
