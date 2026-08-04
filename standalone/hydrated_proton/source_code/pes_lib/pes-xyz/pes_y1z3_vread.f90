SUBROUTINE pes_y1z3_vread (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_y1z3_vpc
read (iun,*) nb
if (nb.ne.pes_y1z3_nvb(pes_y1z3_vpc%dg)) then
 stop 'pes_y1z3_vread: dimension error'
endif
allocate (pes_y1z3_vcf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_y1z3_vcf
endif
close (iun)
return
END SUBROUTINE pes_y1z3_vread
