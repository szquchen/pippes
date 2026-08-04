SUBROUTINE pes_x7y2_vread (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_x7y2_vpc
read (iun,*) nb
if (nb.ne.pes_x7y2_nvb(pes_x7y2_vpc%dg)) then
 stop 'pes_x7y2_vread: dimension error'
endif
allocate (pes_x7y2_vcf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_x7y2_vcf
endif
close (iun)
return
END SUBROUTINE pes_x7y2_vread
