SUBROUTINE pes_x3y1z2_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x3y1z2_nki)) then
 stop 'pes_x3y1z2_getvcf: bad dimension'
else if (any(nki.ne.pes_x3y1z2_nki)) then
 stop 'pes_x3y1z2_getvcf: bad nki'
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
else if (all(nkj.eq.(/1,0,1/))) then
 call pes_x1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1z1', pes_x1z1_vpc, pes_x1z1_vcf)
else if (all(nkj.eq.(/0,1,1/))) then
 call pes_y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1z1', pes_y1z1_vpc, pes_y1z1_vcf)
else if (all(nkj.eq.(/0,0,2/))) then
 call pes_z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-z2', pes_z2_vpc, pes_z2_vcf)
else if (all(nkj.eq.(/3,0,0/))) then
 call pes_x3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3', pes_x3_vpc, pes_x3_vcf)
else if (all(nkj.eq.(/2,1,0/))) then
 call pes_x2y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1', pes_x2y1_vpc, pes_x2y1_vcf)
else if (all(nkj.eq.(/2,0,1/))) then
 call pes_x2z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2z1', pes_x2z1_vpc, pes_x2z1_vcf)
else if (all(nkj.eq.(/1,1,1/))) then
 call pes_x1y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1z1', &
   pes_x1y1z1_vpc, pes_x1y1z1_vcf)
else if (all(nkj.eq.(/1,0,2/))) then
 call pes_x1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1z2', pes_x1z2_vpc, pes_x1z2_vcf)
else if (all(nkj.eq.(/0,1,2/))) then
 call pes_y1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1z2', pes_y1z2_vpc, pes_y1z2_vcf)
else if (all(nkj.eq.(/3,1,0/))) then
 call pes_x3y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1', &
   pes_x3y1_vpc, pes_x3y1_vcf)
else if (all(nkj.eq.(/3,0,1/))) then
 call pes_x3z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3z1', &
   pes_x3z1_vpc, pes_x3z1_vcf)
else if (all(nkj.eq.(/2,1,1/))) then
 call pes_x2y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1z1', &
   pes_x2y1z1_vpc, pes_x2y1z1_vcf)
else if (all(nkj.eq.(/2,0,2/))) then
 call pes_x2z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2z2', &
   pes_x2z2_vpc, pes_x2z2_vcf)
else if (all(nkj.eq.(/1,1,2/))) then
 call pes_x1y1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1z2', &
   pes_x1y1z2_vpc, pes_x1y1z2_vcf)
else if (all(nkj.eq.(/3,1,1/))) then
 call pes_x3y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1z1', &
   pes_x3y1z1_vpc, pes_x3y1z1_vcf)
else if (all(nkj.eq.(/3,0,2/))) then
 call pes_x3z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3z2', &
   pes_x3z2_vpc, pes_x3z2_vcf)
else if (all(nkj.eq.(/2,1,2/))) then
 call pes_x2y1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1z2', &
   pes_x2y1z2_vpc, pes_x2y1z2_vcf)
else if (all(nkj.eq.(/3,2,1/))) then
 call pes_x3y1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1z2', &
   pes_x3y1z2_vpc, pes_x3y1z2_vcf)
else
 stop 'pes_x3y1z2_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_x3y1z2_getvcf
