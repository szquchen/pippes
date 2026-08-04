SUBROUTINE pes_z1_vadd (cf)
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
if (size(cf).eq.1) then
 pes_z1_vcf = pes_z1_vcf+cf(0)
else
 stop 'pes_z1_vadd: size mismatch'
endif
return
END SUBROUTINE pes_z1_vadd
