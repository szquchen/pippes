SUBROUTINE pes_x5y1z1u1_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x5y1z1u1_nki)) then
 stop 'pes_x5y1z1u1_getvcf: bad dimension'
else if (any(nki.ne.pes_x5y1z1u1_nki)) then
 stop 'pes_x5y1z1u1_getvcf: bad nki'
endif
if (all(nkj.eq.(/1,0,0,0/))) then
 call pes_x1_vadd (cf)
 call pes_write0 (iun, 'vpcf-x1', pes_x1_vcf(0))
else if (all(nkj.eq.(/0,1,0,0/))) then
 call pes_y1_vadd (cf)
 call pes_write0 (iun, 'vpcf-y1', pes_y1_vcf(0))
else if (all(nkj.eq.(/0,0,1,0/))) then
 call pes_z1_vadd (cf)
 call pes_write0 (iun, 'vpcf-z1', pes_z1_vcf(0))
else if (all(nkj.eq.(/0,0,0,1/))) then
 call pes_u1_vadd (cf)
 call pes_write0 (iun, 'vpcf-u1', pes_u1_vcf(0))
else if (all(nkj.eq.(/2,0,0,0/))) then
 call pes_x2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2', pes_x2_vpc, pes_x2_vcf)
else if (all(nkj.eq.(/1,1,0,0/))) then
 call pes_x1y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1', pes_x1y1_vpc, pes_x1y1_vcf)
else if (all(nkj.eq.(/1,0,1,0/))) then
 call pes_x1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1z1', pes_x1z1_vpc, pes_x1z1_vcf)
else if (all(nkj.eq.(/0,1,1,0/))) then
 call pes_y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1z1', pes_y1z1_vpc, pes_y1z1_vcf)
else if (all(nkj.eq.(/1,0,0,1/))) then
 call pes_x1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1u1', pes_x1u1_vpc, pes_x1u1_vcf)
else if (all(nkj.eq.(/0,1,0,1/))) then
 call pes_y1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1u1', pes_y1u1_vpc, pes_y1u1_vcf)
else if (all(nkj.eq.(/0,0,1,1/))) then
 call pes_z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-z1u1', pes_z1u1_vpc, pes_z1u1_vcf)
else if (all(nkj.eq.(/3,0,0,0/))) then
 call pes_x3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3', pes_x3_vpc, pes_x3_vcf)
else if (all(nkj.eq.(/2,1,0,0/))) then
 call pes_x2y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1', pes_x2y1_vpc, pes_x2y1_vcf)
else if (all(nkj.eq.(/2,0,1,0/))) then
 call pes_x2z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2z1', pes_x2z1_vpc, pes_x2z1_vcf)
else if (all(nkj.eq.(/1,1,1,0/))) then
 call pes_x1y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1z1', pes_x1y1z1_vpc, pes_x1y1z1_vcf)
else if (all(nkj.eq.(/2,0,0,1/))) then
 call pes_x2u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2u1', pes_x2u1_vpc, pes_x2u1_vcf)
else if (all(nkj.eq.(/1,1,0,1/))) then
 call pes_x1y1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1u1', pes_x1y1u1_vpc, pes_x1y1u1_vcf)
else if (all(nkj.eq.(/1,0,1,1/))) then
 call pes_x1z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1z1u1', pes_x1z1u1_vpc, pes_x1z1u1_vcf)
else if (all(nkj.eq.(/0,1,1,1/))) then
 call pes_y1z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1z1u1', pes_y1z1u1_vpc, pes_y1z1u1_vcf)
else if (all(nkj.eq.(/4,0,0,0/))) then
 call pes_x4_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4', pes_x4_vpc, pes_x4_vcf)
else if (all(nkj.eq.(/3,1,0,0/))) then
 call pes_x3y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1', pes_x3y1_vpc, pes_x3y1_vcf)
else if (all(nkj.eq.(/3,0,1,0/))) then
 call pes_x3z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3z1', pes_x3z1_vpc, pes_x3z1_vcf)
else if (all(nkj.eq.(/2,1,1,0/))) then
 call pes_x2y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1z1', pes_x2y1z1_vpc, pes_x2y1z1_vcf)
else if (all(nkj.eq.(/3,0,0,1/))) then
 call pes_x3u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3u1', pes_x3u1_vpc, pes_x3u1_vcf)
else if (all(nkj.eq.(/2,1,0,1/))) then
 call pes_x2y1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1u1', pes_x2y1u1_vpc, pes_x2y1u1_vcf)
else if (all(nkj.eq.(/2,0,1,1/))) then
 call pes_x2z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2z1u1', pes_x2z1u1_vpc, pes_x2z1u1_vcf)
else if (all(nkj.eq.(/1,1,1,1/))) then
 call pes_x1y1z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1y1z1u1', &
   pes_x1y1z1u1_vpc, pes_x1y1z1u1_vcf)
else if (all(nkj.eq.(/5,0,0,0/))) then
 call pes_x5_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5', pes_x5_vpc, pes_x5_vcf)
else if (all(nkj.eq.(/4,1,0,0/))) then
 call pes_x4y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4y1', pes_x4y1_vpc, pes_x4y1_vcf)
else if (all(nkj.eq.(/4,0,1,0/))) then
 call pes_x4z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4z1', pes_x4z1_vpc, pes_x4z1_vcf)
else if (all(nkj.eq.(/3,1,1,0/))) then
 call pes_x3y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1z1', pes_x3y1z1_vpc, pes_x3y1z1_vcf)
else if (all(nkj.eq.(/4,0,0,1/))) then
 call pes_x4u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4u1', pes_x4u1_vpc, pes_x4u1_vcf)
else if (all(nkj.eq.(/3,1,0,1/))) then
 call pes_x3y1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1u1', pes_x3y1u1_vpc, pes_x3y1u1_vcf)
else if (all(nkj.eq.(/3,0,1,1/))) then
 call pes_x3z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3z1u1', pes_x3z1u1_vpc, pes_x3z1u1_vcf)
else if (all(nkj.eq.(/2,1,1,1/))) then
 call pes_x2y1z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2y1z1u1', &
   pes_x2y1z1u1_vpc, pes_x2y1z1u1_vcf)
else if (all(nkj.eq.(/5,1,0,0/))) then
 call pes_x5y1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5y1', pes_x5y1_vpc, pes_x5y1_vcf)
else if (all(nkj.eq.(/5,0,1,0/))) then
 call pes_x5z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5z1', pes_x5z1_vpc, pes_x5z1_vcf)
else if (all(nkj.eq.(/4,1,1,0/))) then
 call pes_x4y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4y1z1', pes_x4y1z1_vpc, pes_x4y1z1_vcf)
else if (all(nkj.eq.(/5,0,0,1/))) then
 call pes_x5u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5u1', pes_x5u1_vpc, pes_x5u1_vcf)
else if (all(nkj.eq.(/4,1,0,1/))) then
 call pes_x4y1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4y1u1', pes_x4y1u1_vpc, pes_x4y1u1_vcf)
else if (all(nkj.eq.(/4,0,1,1/))) then
 call pes_x4z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4z1u1', pes_x4z1u1_vpc, pes_x4z1u1_vcf)
else if (all(nkj.eq.(/3,1,1,1/))) then
 call pes_x3y1z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3y1z1u1', &
   pes_x3y1z1u1_vpc, pes_x3y1z1u1_vcf)
else if (all(nkj.eq.(/5,1,1,0/))) then
 call pes_x5y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5y1z1', pes_x5y1z1_vpc, pes_x5y1z1_vcf)
else if (all(nkj.eq.(/5,1,0,1/))) then
 call pes_x5y1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5y1u1', pes_x5y1u1_vpc, pes_x5y1u1_vcf)
else if (all(nkj.eq.(/5,0,1,1/))) then
 call pes_x5z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5z1u1', pes_x5z1u1_vpc, pes_x5z1u1_vcf)
else if (all(nkj.eq.(/4,1,1,1/))) then
 call pes_x4y1z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x4y1z1u1', pes_x4y1z1u1_vpc, pes_x4y1z1u1_vcf)
else if (all(nkj.eq.(/5,1,1,1/))) then
 call pes_x5y1z1u1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x5y1z1u1', pes_x5y1z1u1_vpc, pes_x5y1z1u1_vcf)
else
 stop 'pes_x5y1z1u1_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_x5y1z1u1_getvcf
