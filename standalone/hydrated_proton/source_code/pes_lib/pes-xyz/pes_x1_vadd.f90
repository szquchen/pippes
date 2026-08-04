SUBROUTINE pes_x1_vadd (cf)
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
if (size(cf).eq.1) then
 pes_x1_vcf = pes_x1_vcf+cf(0)
else
 stop 'pes_x1_vadd: size mismatch'
endif
return
END SUBROUTINE pes_x1_vadd
