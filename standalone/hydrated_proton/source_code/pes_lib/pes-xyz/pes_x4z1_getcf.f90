SUBROUTINE pes_x4z1_getcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x4z1_nki)) then
 stop 'pes_x4z1_getcf: bad dimension'
else if (any(nki.ne.pes_x4z1_nki)) then
 stop 'pes_x4z1_getcf: bad nki'
endif
if (all(nkj.eq.(/1,0/))) then
 call pes_x1_add (cf)
 call pes_write0 (iun, 'pcf-x1', pes_x1_cf)
else if (all(nkj.eq.(/0,1/))) then
 call pes_z1_add (cf)
 call pes_write0 (iun, 'pcf-z1', pes_z1_cf)
else if (all(nkj.eq.(/2,0/))) then
 call pes_x2_add (pc, cf)
 call pes_write (iun, 'pcf-x2', pes_x2_pc, pes_x2_cf)
else if (all(nkj.eq.(/1,1/))) then
 call pes_x1z1_add (pc, cf)
 call pes_write (iun, 'pcf-x1z1', pes_x1z1_pc, pes_x1z1_cf)
else if (all(nkj.eq.(/3,0/))) then
 call pes_x3_add (pc, cf)
 call pes_write (iun, 'pcf-x3', pes_x3_pc, pes_x3_cf)
else if (all(nkj.eq.(/2,1/))) then
 call pes_x2z1_add (pc, cf)
 call pes_write (iun, 'pcf-x2z1', pes_x2z1_pc, pes_x2z1_cf)
else if (all(nkj.eq.(/4,0/))) then
 call pes_x4_add (pc, cf)
 call pes_write (iun, 'pcf-x4', pes_x4_pc, pes_x4_cf)
else if (all(nkj.eq.(/3,1/))) then
 call pes_x3z1_add (pc, cf)
 call pes_write (iun, 'pcf-x3z1', pes_x3z1_pc, pes_x3z1_cf)
else if (all(nkj.eq.(/4,1/))) then
 call pes_x4z1_add (pc, cf)
 call pes_write (iun, 'pcf-x4z1', pes_x4z1_pc, pes_x4z1_cf)
else
 stop 'pes_x4z1_getcf: bad nkj'
end if
return
END SUBROUTINE pes_x4z1_getcf
