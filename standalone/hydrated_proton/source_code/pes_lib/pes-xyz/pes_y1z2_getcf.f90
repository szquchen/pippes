SUBROUTINE pes_y1z2_getcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_y1z2_nki)) then
 stop 'pes_y1z2_getcf: bad dimension'
else if (any(nki.ne.pes_y1z2_nki)) then
 stop 'pes_y1z2_getcf: bad nki'
endif
if (all(nkj.eq.(/1,0/))) then
 call pes_y1_add (cf)
 call pes_write0 (iun, 'pcf-y1', pes_y1_cf)
else if (all(nkj.eq.(/0,1/))) then
 call pes_z1_add (cf)
 call pes_write0 (iun, 'pcf-z1', pes_z1_cf)
else if (all(nkj.eq.(/1,1/))) then
 call pes_y1z1_add (pc, cf)
 call pes_write (iun, 'pcf-y1z1', pes_y1z1_pc, pes_y1z1_cf)
else if (all(nkj.eq.(/0,2/))) then
 call pes_z2_add (pc, cf)
 call pes_write (iun, 'pcf-z2', pes_z2_pc, pes_z2_cf)
else if (all(nkj.eq.(/1,2/))) then
 call pes_y1z2_add (pc, cf)
 call pes_write (iun, 'pcf-y1z2', pes_y1z2_pc, pes_y1z2_cf)
else
 stop 'pes_y1z2_getcf: bad nkj'
end if
return
END SUBROUTINE pes_y1z2_getcf
