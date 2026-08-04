SUBROUTINE pes_x2_getvcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getvcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x2_nki)) then
 stop 'pes_x2_getvcf: bad dimension'
else if (any(nki.ne.pes_x2_nki)) then
 stop 'pes_x2_getvcf: bad nki'
endif
if (all(nkj.eq.(/1/))) then
 call pes_x1_vadd (cf)
 call pes_write0 (iun, 'vpcf-x1', pes_x1_vcf(0))
else if (all(nkj.eq.(/2/))) then
 call pes_x2_vadd (pc, cf)
 call pes_write (iun, 'vpcf-x2', pes_x2_vpc, pes_x2_vcf)
else
 stop 'pes_x2_getvcf: bad nkj'
end if
return
END SUBROUTINE pes_x2_getvcf
