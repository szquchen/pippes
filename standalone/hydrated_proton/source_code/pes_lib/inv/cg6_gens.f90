SUBROUTINE cg6_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.nr) then
 stop 'cg6_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the vertices
! (01, 02, 12, 03, 13, 23, 04, 14, 24, 34, 05, 15, 25, 35, 45 ) =>
! (01, 12, 02, 13, 03, 23, 14, 04, 24, 34, 15, 05, 25, 35, 45 )
 iord = (/ 0, 2, 1, 4, 3, 5, 7, 6, 8, 9, 11, 10, 12, 13, 14 /)
case (1)
! permutation (0,1,2,3,4,5) on the vertices
! (01, 02, 12, 03, 13, 23, 04, 14, 24, 34, 05, 15, 25, 35, 45 ) =>
! (12, 13, 23, 14, 24, 34, 15, 25, 35, 45, 01, 02, 03, 04, 05 )
 iord = (/ 2, 4, 5, 7, 8, 9, 11, 12, 13, 14, 0, 1, 3, 6, 10 /)
case default
 stop 'cg6_gens: invalid index'
end select
return
END SUBROUTINE cg6_gens
