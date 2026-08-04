SUBROUTINE pes_x9y4_getcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x9y4_nki)) then
 stop 'pes_x9y4_getcf: bad dimension'
else if (any(nki.ne.pes_x9y4_nki)) then
 stop 'pes_x9y4_getcf: bad nki'
endif
if (all(nkj.eq.(/1,0/))) then
 call pes_x1_add (cf)
 call pes_write0 (iun, 'pcf-x1', pes_x1_cf)
else if (all(nkj.eq.(/0,1/))) then
 call pes_y1_add (cf)
 call pes_write0 (iun, 'pcf-y1', pes_y1_cf)
else if (all(nkj.eq.(/2,0/))) then
 call pes_x2_add (pc, cf)
 call pes_write (iun, 'pcf-x2', pes_x2_pc, pes_x2_cf)
else if (all(nkj.eq.(/1,1/))) then
 call pes_x1y1_add (pc, cf)
 call pes_write (iun, 'pcf-x1y1', pes_x1y1_pc, pes_x1y1_cf)
else if (all(nkj.eq.(/0,2/))) then
 call pes_y2_add (pc, cf)
 call pes_write (iun, 'pcf-y2', pes_y2_pc, pes_y2_cf)
else if (all(nkj.eq.(/3,0/))) then
 call pes_x3_add (pc, cf)
 call pes_write (iun, 'pcf-x3', pes_x3_pc, pes_x3_cf)
else if (all(nkj.eq.(/2,1/))) then
 call pes_x2y1_add (pc, cf)
 call pes_write (iun, 'pcf-x2y1', pes_x2y1_pc, pes_x2y1_cf)
else if (all(nkj.eq.(/1,2/))) then
 call pes_x1y2_add (pc, cf)
 call pes_write (iun, 'pcf-x1y2', pes_x1y2_pc, pes_x1y2_cf)
!!else if (all(nkj.eq.(/0,3/))) then
!! call pes_y3_add (pc, cf)
!! call pes_write (iun, 'pcf-y3', pes_y3_pc, pes_y3_cf)
else if (all(nkj.eq.(/4,0/))) then
 call pes_x4_add (pc, cf)
 call pes_write (iun, 'pcf-x4', pes_x4_pc, pes_x4_cf)
else if (all(nkj.eq.(/3,1/))) then
 call pes_x3y1_add (pc, cf)
 call pes_write (iun, 'pcf-x3y1', pes_x3y1_pc, pes_x3y1_cf)
else if (all(nkj.eq.(/2,2/))) then
 call pes_x2y2_add (pc, cf)
 call pes_write (iun, 'pcf-x2y2', pes_x2y2_pc, pes_x2y2_cf)
!!else if (all(nkj.eq.(/1,3/))) then
!! call pes_x1y3_add (pc, cf)
!! call pes_write (iun, 'pcf-x1y3', pes_x1y3_pc, pes_x1y3_cf)
else if (all(nkj.eq.(/5,0/))) then
 call pes_x5_add (pc, cf)
 call pes_write (iun, 'pcf-x5', pes_x5_pc, pes_x5_cf)
else if (all(nkj.eq.(/4,1/))) then
 call pes_x4y1_add (pc, cf)
 call pes_write (iun, 'pcf-x4y1', pes_x4y1_pc, pes_x4y1_cf)
else if (all(nkj.eq.(/3,2/))) then
 call pes_x3y2_add (pc, cf)
 call pes_write (iun, 'pcf-x3y2', pes_x3y2_pc, pes_x3y2_cf)
!!else if (all(nkj.eq.(/2,3/))) then
!! call pes_x2y3_add (pc, cf)
!! call pes_write (iun, 'pcf-x2y3', pes_x2y3_pc, pes_x2y3_cf)
!!else if (all(nkj.eq.(/6,0/))) then
!! call pes_x6_add (pc, cf)
!! call pes_write (iun, 'pcf-x6', pes_x6_pc, pes_x6_cf)
else if (all(nkj.eq.(/5,1/))) then
 call pes_x5y1_add (pc, cf)
 call pes_write (iun, 'pcf-x5y1', pes_x5y1_pc, pes_x5y1_cf)
else if (all(nkj.eq.(/4,2/))) then
 call pes_x4y2_add (pc, cf)
 call pes_write (iun, 'pcf-x4y2', pes_x4y2_pc, pes_x4y2_cf)
!!else if (all(nkj.eq.(/3,3/))) then
!! call pes_x3y3_add (pc, cf)
!! call pes_write (iun, 'pcf-x3y3', pes_x3y3_pc, pes_x3y3_cf)
!!else if (all(nkj.eq.(/7,0/))) then
!! call pes_x7_add (pc, cf)
!! call pes_write (iun, 'pcf-x7', pes_x7_pc, pes_x7_cf)
!!else if (all(nkj.eq.(/6,1/))) then
!! call pes_x6y1_add (pc, cf)
!! call pes_write (iun, 'pcf-x6y1', pes_x6y1_pc, pes_x6y1_cf)
else if (all(nkj.eq.(/5,2/))) then
 call pes_x5y2_add (pc, cf)
 call pes_write (iun, 'pcf-x5y2', pes_x5y2_pc, pes_x5y2_cf)
!!else if (all(nkj.eq.(/4,3/))) then
!! call pes_x4y3_add (pc, cf)
!! call pes_write (iun, 'pcf-x4y3', pes_x4y3_pc, pes_x4y3_cf)
!!else if (all(nkj.eq.(/7,1/))) then
!! call pes_x7y1_add (pc, cf)
!! call pes_write (iun, 'pcf-x7y1', pes_x7y1_pc, pes_x7y1_cf)
!!else if (all(nkj.eq.(/6,2/))) then
!! call pes_x6y2_add (pc, cf)
!! call pes_write (iun, 'pcf-x6y2', pes_x6y2_pc, pes_x6y2_cf)
!!else if (all(nkj.eq.(/5,3/))) then
!! call pes_x5y3_add (pc, cf)
!! call pes_write (iun, 'pcf-x5y3', pes_x5y3_pc, pes_x5y3_cf)
!!else if (all(nkj.eq.(/7,2/))) then
!! call pes_x7y2_add (pc, cf)
!! call pes_write (iun, 'pcf-x7y2', pes_x7y2_pc, pes_x7y2_cf)
!!else if (all(nkj.eq.(/6,3/))) then
!! call pes_x6y3_add (pc, cf)
!! call pes_write (iun, 'pcf-x6y3', pes_x6y3_pc, pes_x6y3_cf)
!!else if (all(nkj.eq.(/7,3/))) then
!! call pes_x7y3_add (pc, cf)
!! call pes_write (iun, 'pcf-x7y3', pes_x7y3_pc, pes_x7y3_cf)
else
 stop 'pes_x9y4_getcf: bad nkj'
end if
return
END SUBROUTINE pes_x9y4_getcf
