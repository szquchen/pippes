SUBROUTINE cg7_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.nr) then
 stop 'cg7_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the vertices
! (01, 02, 12, 03, 13, 23, 04, 14, 24, 34, 05, 15, 25, 35, 45, 06, ... ) =>
! (01, 12, 02, 13, 03, 23, 14, 04, 24, 34, 15, 05, 25, 35, 45, 16, ... )
 iord = (/ 0, 2, 1, 4, 3, 5, 7, 6, 8, 9, 11, 10, 12, 13, 14, 16, 15, 17, &
  18, 19, 20 /)
case (1)
! permutation (0,1,2,3,4,5,6) on the vertices
! (01, 02, 12, 03, 13, 23, 04, 14, 24, 34, 05, 15, 25, 35, 45, 06, ... ) =>
! (12, 13, 23, 14, 24, 34, 15, 25, 35, 45, 16, 26, 36, 46, 56, 01, ... )
 iord = (/ 2, 4, 5, 7, 8, 9, 11, 12, 13, 14, 16, 17, 18, 19, 20, 0, 1, 3, &
  6, 10, 15 /)
case default
 stop 'cg7_gens: invalid index'
end select
return
END SUBROUTINE cg7_gens
