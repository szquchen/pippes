SUBROUTINE pes_x3y3z3_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x3y3z3_nki)) then
 stop 'pes_x3y3z3_getvcf: bad dimension'
else if (any(nki.ne.pes_x3y3z3_nki)) then
 stop 'pes_x3y3z3_getvcf: bad nki'
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
 call pes_write (iun, 'vpcf-z2', pes_z2_vpc, pes_z2_vcf)
else if (all(nkj.eq.(/3,0,0/))) then
 call pes_x3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3', pes_x3_vpc, pes_x3_vcf)
else if (all(nkj.eq.(/2,1,0/))) then
 call pes_x2y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1', pes_x2y1_vpc, pes_x2y1_vcf)
else if (all(nkj.eq.(/1,2,0/))) then
 call pes_x1y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y2', pes_x1y2_vpc, pes_x1y2_vcf)
else if (all(nkj.eq.(/0,3,0/))) then
 call pes_y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y3', pes_y3_vpc, pes_y3_vcf)
else if (all(nkj.eq.(/2,0,1/))) then
 call pes_x2z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2z1', pes_x2z1_vpc, pes_x2z1_vcf)
else if (all(nkj.eq.(/1,1,1/))) then
 call pes_x1y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1z1', pes_x1y1z1_vpc, pes_x1y1z1_vcf)
else if (all(nkj.eq.(/0,2,1/))) then
 call pes_y2z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y2z1', pes_y2z1_vpc, pes_y2z1_vcf)
else if (all(nkj.eq.(/1,0,2/))) then
 call pes_x1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1z2', pes_x1z2_vpc, pes_x1z2_vcf)
else if (all(nkj.eq.(/0,1,2/))) then
 call pes_y1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1z2', pes_y1z2_vpc, pes_y1z2_vcf)
else if (all(nkj.eq.(/0,0,3/))) then
 call pes_z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-z3', pes_z3_vpc, pes_z3_vcf)
else if (all(nkj.eq.(/3,1,0/))) then
 call pes_x3y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1', pes_x3y1_vpc, pes_x3y1_vcf)
else if (all(nkj.eq.(/2,2,0/))) then
 call pes_x2y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y2', pes_x2y2_vpc, pes_x2y2_vcf)
else if (all(nkj.eq.(/1,3,0/))) then
 call pes_x1y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y3', pes_x1y3_vpc, pes_x1y3_vcf)
else if (all(nkj.eq.(/3,0,1/))) then
 call pes_x3z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3z1', pes_x3z1_vpc, pes_x3z1_vcf)
else if (all(nkj.eq.(/2,1,1/))) then
 call pes_x2y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1z1', pes_x2y1z1_vpc, pes_x2y1z1_vcf)
else if (all(nkj.eq.(/1,2,1/))) then
 call pes_x1y2z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y2z1', pes_x1y2z1_vpc, pes_x1y2z1_vcf)
else if (all(nkj.eq.(/0,3,1/))) then
 call pes_y3z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y3z1', pes_y3z1_vpc, pes_y3z1_vcf)
else if (all(nkj.eq.(/2,0,2/))) then
 call pes_x2z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2z2', pes_x2z2_vpc, pes_x2z2_vcf)
else if (all(nkj.eq.(/1,1,2/))) then
 call pes_x1y1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1z2', pes_x1y1z2_vpc, pes_x1y1z2_vcf)
else if (all(nkj.eq.(/0,2,2/))) then
 call pes_y2z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y2z2', pes_y2z2_vpc, pes_y2z2_vcf)
else if (all(nkj.eq.(/1,0,3/))) then
 call pes_x1z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1z3', pes_x1z3_vpc, pes_x1z3_vcf)
else if (all(nkj.eq.(/0,1,3/))) then
 call pes_y1z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1z3', pes_y1z3_vpc, pes_y1z3_vcf)
else if (all(nkj.eq.(/3,2,0/))) then
 call pes_x3y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y2', pes_x3y2_vpc, pes_x3y2_vcf)
else if (all(nkj.eq.(/2,3,0/))) then
 call pes_x2y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y3', pes_x2y3_vpc, pes_x2y3_vcf)
else if (all(nkj.eq.(/3,1,1/))) then
 call pes_x3y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1z1', pes_x3y1z1_vpc, pes_x3y1z1_vcf)
else if (all(nkj.eq.(/2,2,1/))) then
 call pes_x2y2z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y2z1', pes_x2y2z1_vpc, pes_x2y2z1_vcf)
else if (all(nkj.eq.(/1,3,1/))) then
 call pes_x1y3z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y3z1', pes_x1y3z1_vpc, pes_x1y3z1_vcf)
else if (all(nkj.eq.(/3,0,2/))) then
 call pes_x3z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3z2', pes_x3z2_vpc, pes_x3z2_vcf)
else if (all(nkj.eq.(/2,1,2/))) then
 call pes_x2y1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1z2', pes_x2y1z2_vpc, pes_x2y1z2_vcf)
else if (all(nkj.eq.(/1,2,2/))) then
 call pes_x1y2z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y2z2', pes_x1y2z2_vpc, pes_x1y2z2_vcf)
else if (all(nkj.eq.(/0,3,2/))) then
 call pes_y3z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y3z2', pes_y3z2_vpc, pes_y3z2_vcf)
else if (all(nkj.eq.(/2,0,3/))) then
 call pes_x2z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2z3', pes_x2z3_vpc, pes_x2z3_vcf)
else if (all(nkj.eq.(/1,1,3/))) then
 call pes_x1y1z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1z3', pes_x1y1z3_vpc, pes_x1y1z3_vcf)
else if (all(nkj.eq.(/0,2,3/))) then
 call pes_y2z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y2z3', pes_y2z3_vpc, pes_y2z3_vcf)
else if (all(nkj.eq.(/3,3,0/))) then
 call pes_x3y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y3', pes_x3y3_vpc, pes_x3y3_vcf)
else if (all(nkj.eq.(/3,2,1/))) then
 call pes_x3y2z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y2z1', pes_x3y2z1_vpc, pes_x3y2z1_vcf)
else if (all(nkj.eq.(/2,3,1/))) then
 call pes_x2y3z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y3z1', pes_x2y3z1_vpc, pes_x2y3z1_vcf)
else if (all(nkj.eq.(/3,1,2/))) then
 call pes_x3y1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1z2', pes_x3y1z2_vpc, pes_x3y1z2_vcf)
else if (all(nkj.eq.(/2,2,2/))) then
 call pes_x2y2z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y2z2', pes_x2y2z2_vpc, pes_x2y2z2_vcf)
else if (all(nkj.eq.(/1,3,2/))) then
 call pes_x1y3z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y3z2', pes_x1y3z2_vpc, pes_x1y3z2_vcf)
else if (all(nkj.eq.(/3,0,3/))) then
 call pes_x3z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3z3', pes_x3z3_vpc, pes_x3z3_vcf)
else if (all(nkj.eq.(/2,1,3/))) then
 call pes_x2y1z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1z3', pes_x2y1z3_vpc, pes_x2y1z3_vcf)
else if (all(nkj.eq.(/1,2,3/))) then
 call pes_x1y2z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y2z3', pes_x1y2z3_vpc, pes_x1y2z3_vcf)
else if (all(nkj.eq.(/0,3,3/))) then
 call pes_y3z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y3z3', pes_y3z3_vpc, pes_y3z3_vcf)
else if (all(nkj.eq.(/3,3,1/))) then
 call pes_x3y3z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y3z1', pes_x3y3z1_vpc, pes_x3y3z1_vcf)
else if (all(nkj.eq.(/3,2,2/))) then
 call pes_x3y2z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y2z2', pes_x3y2z2_vpc, pes_x3y2z2_vcf)
else if (all(nkj.eq.(/2,3,2/))) then
 call pes_x2y3z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y3z2', pes_x2y3z2_vpc, pes_x2y3z2_vcf)
else if (all(nkj.eq.(/3,1,3/))) then
 call pes_x3y1z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1z3', pes_x3y1z3_vpc, pes_x3y1z3_vcf)
else if (all(nkj.eq.(/2,2,3/))) then
 call pes_x2y2z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y2z3', pes_x2y2z3_vpc, pes_x2y2z3_vcf)
else if (all(nkj.eq.(/1,3,3/))) then
 call pes_x1y3z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y3z3', pes_x1y3z3_vpc, pes_x1y3z3_vcf)
else if (all(nkj.eq.(/3,3,2/))) then
 call pes_x3y3z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y3z2', pes_x3y3z2_vpc, pes_x3y3z2_vcf)
else if (all(nkj.eq.(/3,2,3/))) then
 call pes_x3y2z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y2z3', pes_x3y2z3_vpc, pes_x3y2z3_vcf)
else if (all(nkj.eq.(/2,3,3/))) then
 call pes_x2y3z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y3z3', pes_x2y3z3_vpc, pes_x2y3z3_vcf)
else if (all(nkj.eq.(/3,3,3/))) then
 call pes_x3y3z3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y3z3', pes_x3y3z3_vpc, pes_x3y3z3_vcf)
else
 stop 'pes_x3y3z3_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_x3y3z3_getvcf
