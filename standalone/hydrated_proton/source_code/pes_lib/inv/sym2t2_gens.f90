SUBROUTINE sym2t2_gens (ind, iord)
integer, intent (in) :: ind
integer, intent (out) :: iord(0:)
!-----------------------------------------------------------------------
if (size(iord).ne.sym2t2_nr) then
 stop 'sym2t2_gens: bad size iord'
endif
select case (ind)
case (0)
! permutation (0,1) on the left of the s2 part
! (00, 10) =>
! (10, 00)
 iord = (/ 1, 0, 2, 3 /)
case default
 stop 'sym2t2_gens: invalid index'
end select
return
END SUBROUTINE sym2t2_gens
