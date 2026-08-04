SUBROUTINE pes_x4y4z2_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x4y4z2_nki)) then
 stop 'pes_x4y4z2_getvcf: bad dimension'
else if (any(nki.ne.pes_x4y4z2_nki)) then
 stop 'pes_x4y4z2_getvcf: bad nki'
endif
if (all(nkj.eq.(/1,0,0/))) then
 call pes_x1_vadd (cf)
 call pes_write0 (iun, 'vpcf-x1', pes_x1_vcf(0))
else if (all(nkj.eq.(/0,1,0/))) then
 call pes_y1_vadd (cf)
 call pes_write0 (iun, 'vpcf-y1', pes_y1_vcf(0))
else if (all(nkj.eq.(/0,0,1/))) then
 call pes_z1_vadd (cf)
 call pes_write0 (iun, 'vpcf-z1', pes_z1_vcf(0))
else if (all(nkj.eq.(/2,0,0/))) then
 call pes_x2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2', pes_x2_vpc, pes_x2_vcf)
else if (all(nkj.eq.(/1,1,0/))) then
 call pes_x1y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1', pes_x1y1_vpc, pes_x1y1_vcf)
else if (all(nkj.eq.(/0,2,0/))) then
 call pes_y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y2', pes_y2_vpc, pes_y2_vcf)
else if (all(nkj.eq.(/1,0,1/))) then
 call pes_x1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1z1', pes_x1z1_vpc, pes_x1z1_vcf)
else if (all(nkj.eq.(/0,1,1/))) then
 call pes_y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1z1', pes_y1z1_vpc, pes_y1z1_vcf)
else if (all(nkj.eq.(/0,0,2/))) then
 call pes_z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-z2', &
   pes_z2_vpc, pes_z2_vcf)
else if (all(nkj.eq.(/4,4,2/))) then
 call pes_x4y4z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4y4z2', &
   pes_x4y4z2_vpc, pes_x4y4z2_vcf)
else
 stop 'pes_x4y4z2_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_x4y4z2_getvcf
