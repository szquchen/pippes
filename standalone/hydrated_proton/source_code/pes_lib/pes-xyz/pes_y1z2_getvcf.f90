SUBROUTINE pes_y1z2_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_y1z2_nki)) then
 stop 'pes_y1z2_getvcf: bad dimension'
else if (any(nki.ne.pes_y1z2_nki)) then
 stop 'pes_y1z2_getvcf: bad nki'
endif
if (all(nkj.eq.(/1,0/))) then
 call pes_y1_vadd (cf)
 call pes_write0 (iun, 'vpcf-y1', pes_y1_vcf(0))
else if (all(nkj.eq.(/0,1/))) then
 call pes_z1_vadd (cf)
 call pes_write0 (iun, 'vpcf-z1', pes_z1_vcf(0))
else if (all(nkj.eq.(/1,1/))) then
 call pes_y1z1_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1z1', pes_y1z1_vpc, pes_y1z1_vcf)
else if (all(nkj.eq.(/0,2/))) then
 call pes_z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-z2', pes_z2_vpc, pes_z2_vcf)
else if (all(nkj.eq.(/1,2/))) then
 call pes_y1z2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y1z2', pes_y1z2_vpc, pes_y1z2_vcf)
else
 stop 'pes_y1z2_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_y1z2_getvcf
