SUBROUTINE pes_y1_vadd (cf)
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
if (size(cf).eq.1) then
 pes_y1_vcf = pes_y1_vcf+cf(0)
else
 stop 'pes_y1_vadd: size mismatch'
endif
return
END SUBROUTINE pes_y1_vadd
