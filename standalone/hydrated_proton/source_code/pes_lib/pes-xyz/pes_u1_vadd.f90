SUBROUTINE pes_u1_vadd (cf)
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
if (size(cf).eq.1) then
 pes_u1_vcf = pes_u1_vcf+cf(0)
else
 stop 'pes_u1_vadd: size mismatch'
endif
return
END SUBROUTINE pes_u1_vadd
