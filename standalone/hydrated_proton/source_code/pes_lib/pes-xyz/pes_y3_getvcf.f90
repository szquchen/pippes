SUBROUTINE pes_y3_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_y3_nki)) then
 stop 'pes_y3_getvcf: bad dimension'
else if (any(nki.ne.pes_y3_nki)) then
 stop 'pes_y3_getvcf: bad nki'
endif
if (all(nkj.eq.(/1/))) then
 call pes_y1_vadd (cf)
 call pes_write0 (iun, 'vpcf-y1', pes_y1_vcf(0))
else if (all(nkj.eq.(/2/))) then
 call pes_y2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y2', pes_y2_vpc, pes_y2_vcf)
else if (all(nkj.eq.(/3/))) then
 call pes_y3_vadd (pc, cf)
 call pes_write (iun, 'vpcf-y3', pes_y3_vpc, pes_y3_vcf)
else
 stop 'pes_y3_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_y3_getvcf
