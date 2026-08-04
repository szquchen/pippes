SUBROUTINE pes_x8_vread (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_x8_vpc
read (iun,*) nb
if (nb.ne.pes_x8_nvb(pes_x8_vpc%dg)) then
 stop 'pes_x8_vread: dimension error'
endif
allocate (pes_x8_vcf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_x8_vcf
endif
close (iun)
return
END SUBROUTINE pes_x8_vread
