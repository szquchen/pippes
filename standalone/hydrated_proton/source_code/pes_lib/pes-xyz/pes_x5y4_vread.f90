SUBROUTINE pes_x5y4_vread (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_x5y4_vpc
read (iun,*) nb
if (nb.ne.pes_x5y4_nvb(pes_x5y4_vpc%dg)) then
 stop 'pes_x5y4_vread: dimension error'
endif
allocate (pes_x5y4_vcf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_x5y4_vcf
endif
close (iun)
return
END SUBROUTINE pes_x5y4_vread
