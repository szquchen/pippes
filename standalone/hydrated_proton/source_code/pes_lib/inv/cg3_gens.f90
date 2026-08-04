SUBROUTINE cg3_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.nr) then
 stop 'cg3_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the vertices
! (01, 02, 12) =>
! (01, 12, 02)
 iord = (/ 0, 2, 1 /)
case (1)
! permutation (0,1,2) on the vertices
! (01, 02, 12) =>
! (12, 01, 02)
 iord = (/ 2, 0, 1 /)
case default
 stop 'cg3_gens: invalid index'
end select
return
END SUBROUTINE cg3_gens
