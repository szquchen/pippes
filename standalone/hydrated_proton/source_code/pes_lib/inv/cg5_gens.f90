SUBROUTINE cg5_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.nr) then
 stop 'cg5_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the vertices
! (01, 02, 12, 03, 13, 23, 04, 14, 24, 34 ) =>
! (01, 12, 02, 13, 03, 23, 14, 04, 24, 34 )
 iord = (/ 0, 2, 1, 4, 3, 5, 7, 6, 8, 9 /)
case (1)
! permutation (0,1,2,3,4) on the vertices
! (01, 02, 12, 03, 13, 23, 04, 14, 24, 34 ) =>
! (12, 13, 23, 14, 24, 34, 01, 02, 03, 04 )
 iord = (/ 2, 4, 5, 7, 8, 9, 0, 1, 3, 6 /)
case default
 stop 'cg5_gens: invalid index'
end select
return
END SUBROUTINE cg5_gens
