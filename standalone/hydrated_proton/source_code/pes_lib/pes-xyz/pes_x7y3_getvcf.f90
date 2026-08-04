SUBROUTINE pes_x7y3_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x7y3_nki)) then
 stop 'pes_x7y3_getvcf: bad dimension'
else if (any(nki.ne.pes_x7y3_nki)) then
 stop 'pes_x7y3_getvcf: bad nki'
endif
if (all(nkj.eq.(/1,0/))) then
 call pes_x1_vadd (cf)
 call pes_write0 (iun, 'vpcf-x1', pes_x1_vcf(0))
else if (all(nkj.eq.(/0,1/))) then
 call pes_y1_vadd (cf)
 call pes_write0 (iun, 'vpcf-y1', pes_y1_vcf(0))
else if (all(nkj.eq.(/2,0/))) then
 call pes_x2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2', pes_x2_vpc, pes_x2_vcf)
else if (all(nkj.eq.(/1,1/))) then
 call pes_x1y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1', pes_x1y1_vpc, pes_x1y1_vcf)
else if (all(nkj.eq.(/0,2/))) then
 call pes_y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y2', pes_y2_vpc, pes_y2_vcf)
else if (all(nkj.eq.(/3,0/))) then
 call pes_x3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3', pes_x3_vpc, pes_x3_vcf)
else if (all(nkj.eq.(/2,1/))) then
 call pes_x2y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1', pes_x2y1_vpc, pes_x2y1_vcf)
else if (all(nkj.eq.(/1,2/))) then
 call pes_x1y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y2', pes_x1y2_vpc, pes_x1y2_vcf)
else if (all(nkj.eq.(/0,3/))) then
 call pes_y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y3', pes_y3_vpc, pes_y3_vcf)
else if (all(nkj.eq.(/4,0/))) then
 call pes_x4_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4', pes_x4_vpc, pes_x4_vcf)
else if (all(nkj.eq.(/3,1/))) then
 call pes_x3y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1', pes_x3y1_vpc, pes_x3y1_vcf)
else if (all(nkj.eq.(/2,2/))) then
 call pes_x2y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y2', pes_x2y2_vpc, pes_x2y2_vcf)
else if (all(nkj.eq.(/1,3/))) then
 call pes_x1y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y3', pes_x1y3_vpc, pes_x1y3_vcf)
else if (all(nkj.eq.(/5,0/))) then
 call pes_x5_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5', pes_x5_vpc, pes_x5_vcf)
else if (all(nkj.eq.(/4,1/))) then
 call pes_x4y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4y1', pes_x4y1_vpc, pes_x4y1_vcf)
else if (all(nkj.eq.(/3,2/))) then
 call pes_x3y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y2', pes_x3y2_vpc, pes_x3y2_vcf)
else if (all(nkj.eq.(/2,3/))) then
 call pes_x2y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y3', pes_x2y3_vpc, pes_x2y3_vcf)
else if (all(nkj.eq.(/6,0/))) then
 call pes_x6_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x6', pes_x6_vpc, pes_x6_vcf)
else if (all(nkj.eq.(/5,1/))) then
 call pes_x5y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5y1', pes_x5y1_vpc, pes_x5y1_vcf)
else if (all(nkj.eq.(/4,2/))) then
 call pes_x4y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4y2', pes_x4y2_vpc, pes_x4y2_vcf)
else if (all(nkj.eq.(/3,3/))) then
 call pes_x3y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y3', pes_x3y3_vpc, pes_x3y3_vcf)
else if (all(nkj.eq.(/7,0/))) then
 call pes_x7_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x7', pes_x7_vpc, pes_x7_vcf)
else if (all(nkj.eq.(/6,1/))) then
 call pes_x6y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x6y1', pes_x6y1_vpc, pes_x6y1_vcf)
else if (all(nkj.eq.(/5,2/))) then
 call pes_x5y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5y2', pes_x5y2_vpc, pes_x5y2_vcf)
else if (all(nkj.eq.(/4,3/))) then
 call pes_x4y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4y3', pes_x4y3_vpc, pes_x4y3_vcf)
else if (all(nkj.eq.(/7,1/))) then
 call pes_x7y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x7y1', pes_x7y1_vpc, pes_x7y1_vcf)
else if (all(nkj.eq.(/6,2/))) then
 call pes_x6y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x6y2', pes_x6y2_vpc, pes_x6y2_vcf)
else if (all(nkj.eq.(/5,3/))) then
 call pes_x5y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5y3', pes_x5y3_vpc, pes_x5y3_vcf)
else if (all(nkj.eq.(/7,2/))) then
 call pes_x7y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x7y2', pes_x7y2_vpc, pes_x7y2_vcf)
else if (all(nkj.eq.(/6,3/))) then
 call pes_x6y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x6y3', pes_x6y3_vpc, pes_x6y3_vcf)
else if (all(nkj.eq.(/7,3/))) then
 call pes_x7y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x7y3', pes_x7y3_vpc, pes_x7y3_vcf)
else
 stop 'pes_x7y3_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_x7y3_getvcf
