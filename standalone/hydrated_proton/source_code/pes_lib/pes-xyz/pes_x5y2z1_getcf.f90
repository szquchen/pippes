SUBROUTINE pes_x5y2z1_getcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x5y2z1_nki)) then
 stop 'pes_x5y2z1_getcf: bad dimension'
else if (any(nki.ne.pes_x5y2z1_nki)) then
 stop 'pes_x5y2z1_getcf: bad nki'
endif
if (all(nkj.eq.(/1,0,0/))) then
 call pes_x1_add (cf)
 call pes_write0 (iun, 'pcf-x1', pes_x1_cf)
else if (all(nkj.eq.(/0,1,0/))) then
 call pes_y1_add (cf)
 call pes_write0 (iun, 'pcf-y1', pes_y1_cf)
else if (all(nkj.eq.(/0,0,1/))) then
 call pes_z1_add (cf)
 call pes_write0 (iun, 'pcf-z1', pes_z1_cf)
else if (all(nkj.eq.(/2,0,0/))) then
 call pes_x2_add (pc, cf)
 call pes_write (iun, 'pcf-x2', pes_x2_pc, pes_x2_cf)
else if (all(nkj.eq.(/1,1,0/))) then
 call pes_x1y1_add (pc, cf)
 call pes_write (iun, 'pcf-x1y1', pes_x1y1_pc, pes_x1y1_cf)
else if (all(nkj.eq.(/0,2,0/))) then
 call pes_y2_add (pc, cf)
 call pes_write (iun, 'pcf-y2', pes_y2_pc, pes_y2_cf)
else if (all(nkj.eq.(/1,0,1/))) then
 call pes_x1z1_add (pc, cf)
 call pes_write (iun, 'pcf-x1z1', pes_x1z1_pc, pes_x1z1_cf)
else if (all(nkj.eq.(/0,1,1/))) then
 call pes_y1z1_add (pc, cf)
 call pes_write (iun, 'pcf-y1z1', pes_y1z1_pc, pes_y1z1_cf)
else if (all(nkj.eq.(/3,0,0/))) then
 call pes_x3_add (pc, cf)
 call pes_write (iun, 'pcf-x3', pes_x3_pc, pes_x3_cf)
else if (all(nkj.eq.(/2,1,0/))) then
 call pes_x2y1_add (pc, cf)
 call pes_write (iun, 'pcf-x2y1', pes_x2y1_pc, pes_x2y1_cf)
else if (all(nkj.eq.(/1,2,0/))) then
 call pes_x1y2_add (pc, cf)
 call pes_write (iun, 'pcf-x1y2', pes_x1y2_pc, pes_x1y2_cf)
else if (all(nkj.eq.(/2,0,1/))) then
 call pes_x2z1_add (pc, cf)
 call pes_write (iun, 'pcf-x2z1', pes_x2z1_pc, pes_x2z1_cf)
else if (all(nkj.eq.(/1,1,1/))) then
 call pes_x1y1z1_add (pc, cf)
 call pes_write (iun, 'pcf-x1y1z1', pes_x1y1z1_pc, pes_x1y1z1_cf)
else if (all(nkj.eq.(/0,2,1/))) then
 call pes_y2z1_add (pc, cf)
 call pes_write (iun, 'pcf-y2z1', pes_y2z1_pc, pes_y2z1_cf)
else if (all(nkj.eq.(/4,0,0/))) then
 call pes_x4_add (pc, cf)
 call pes_write (iun, 'pcf-x4', pes_x4_pc, pes_x4_cf)
else if (all(nkj.eq.(/3,1,0/))) then
 call pes_x3y1_add (pc, cf)
 call pes_write (iun, 'pcf-x3y1', pes_x3y1_pc, pes_x3y1_cf)
else if (all(nkj.eq.(/2,2,0/))) then
 call pes_x2y2_add (pc, cf)
 call pes_write (iun, 'pcf-x2y2', pes_x2y2_pc, pes_x2y2_cf)
else if (all(nkj.eq.(/3,0,1/))) then
 call pes_x3z1_add (pc, cf)
 call pes_write (iun, 'pcf-x3z1', pes_x3z1_pc, pes_x3z1_cf)
else if (all(nkj.eq.(/2,1,1/))) then
 call pes_x2y1z1_add (pc, cf)
 call pes_write (iun, 'pcf-x2y1z1', pes_x2y1z1_pc, pes_x2y1z1_cf)
else if (all(nkj.eq.(/1,2,1/))) then
 call pes_x1y2z1_add (pc, cf)
 call pes_write (iun, 'pcf-x1y2z1', pes_x1y2z1_pc, pes_x1y2z1_cf)
else if (all(nkj.eq.(/5,0,0/))) then
 call pes_x5_add (pc, cf)
 call pes_write (iun, 'pcf-x5', pes_x5_pc, pes_x5_cf)
else if (all(nkj.eq.(/4,1,0/))) then
 call pes_x4y1_add (pc, cf)
 call pes_write (iun, 'pcf-x4y1', pes_x4y1_pc, pes_x4y1_cf)
else if (all(nkj.eq.(/3,2,0/))) then
 call pes_x3y2_add (pc, cf)
 call pes_write (iun, 'pcf-x3y2', pes_x3y2_pc, pes_x3y2_cf)
else if (all(nkj.eq.(/4,0,1/))) then
 call pes_x4z1_add (pc, cf)
 call pes_write (iun, 'pcf-x4z1', pes_x4z1_pc, pes_x4z1_cf)
else if (all(nkj.eq.(/3,1,1/))) then
 call pes_x3y1z1_add (pc, cf)
 call pes_write (iun, 'pcf-x3y1z1', pes_x3y1z1_pc, pes_x3y1z1_cf)
else if (all(nkj.eq.(/2,2,1/))) then
 call pes_x2y2z1_add (pc, cf)
 call pes_write (iun, 'pcf-x2y2z1', pes_x2y2z1_pc, pes_x2y2z1_cf)
else if (all(nkj.eq.(/5,1,0/))) then
 call pes_x5y1_add (pc, cf)
 call pes_write (iun, 'pcf-x5y1', pes_x5y1_pc, pes_x5y1_cf)
else if (all(nkj.eq.(/4,2,0/))) then
 call pes_x4y2_add (pc, cf)
 call pes_write (iun, 'pcf-x4y2', pes_x4y2_pc, pes_x4y2_cf)
else if (all(nkj.eq.(/5,0,1/))) then
 call pes_x5z1_add (pc, cf)
 call pes_write (iun, 'pcf-x5z1', pes_x5z1_pc, pes_x5z1_cf)
else if (all(nkj.eq.(/4,1,1/))) then
 call pes_x4y1z1_add (pc, cf)
 call pes_write (iun, 'pcf-x4y1z1', pes_x4y1z1_pc, pes_x4y1z1_cf)
else if (all(nkj.eq.(/3,2,1/))) then
 call pes_x3y2z1_add (pc, cf)
 call pes_write (iun, 'pcf-x3y2z1', pes_x3y2z1_pc, pes_x3y2z1_cf)
else if (all(nkj.eq.(/5,2,0/))) then
 call pes_x5y2_add (pc, cf)
 call pes_write (iun, 'pcf-x5y2', pes_x5y2_pc, pes_x5y2_cf)
else if (all(nkj.eq.(/5,1,1/))) then
 call pes_x5y1z1_add (pc, cf)
 call pes_write (iun, 'pcf-x5y1z1', pes_x5y1z1_pc, pes_x5y1z1_cf)
else if (all(nkj.eq.(/4,2,1/))) then
 call pes_x4y2z1_add (pc, cf)
 call pes_write (iun, 'pcf-x4y2z1', pes_x4y2z1_pc, pes_x4y2z1_cf)
else if (all(nkj.eq.(/5,2,1/))) then
 call pes_x5y2z1_add (pc, cf)
 call pes_write (iun, 'pcf-x5y2z1', pes_x5y2z1_pc, pes_x5y2z1_cf)
else
 stop 'pes_x5y2z1_getcf: bad nkj'
end if
return
END SUBROUTINE pes_x5y2z1_getcf
