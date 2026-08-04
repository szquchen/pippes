SUBROUTINE pes_x3z1_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x3z1_nki)) then
 stop 'pes_x3z1_getvcf: bad dimension'
else if (any(nki.ne.pes_x3z1_nki)) then
 stop 'pes_x3z1_getvcf: bad nki'
endif
if (all(nkj.eq.(/1,0/))) then
 call pes_x1_vadd (cf)
 call pes_write0 (iun, 'vpcf-x1', pes_x1_vcf(0))
else if (all(nkj.eq.(/0,1/))) then
 call pes_z1_vadd (cf)
 call pes_write0 (iun, 'vpcf-z1', pes_z1_vcf(0))
else if (all(nkj.eq.(/2,0/))) then
 call pes_x2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2', pes_x2_vpc, pes_x2_vcf)
else if (all(nkj.eq.(/1,1/))) then
 call pes_x1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x1z1', pes_x1z1_vpc, pes_x1z1_vcf)
else if (all(nkj.eq.(/3,0/))) then
 call pes_x3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3', pes_x3_vpc, pes_x3_vcf)
else if (all(nkj.eq.(/2,1/))) then
 call pes_x2z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2z1', pes_x2z1_vpc, pes_x2z1_vcf)
else if (all(nkj.eq.(/3,1/))) then
 call pes_x3z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x3z1', pes_x3z1_vpc, pes_x3z1_vcf)
else
 stop 'pes_x3z1_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_x3z1_getvcf
