SUBROUTINE pes_x3y3z1_vread (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_x3y3z1_vpc
read (iun,*) nb
if (nb.ne.pes_x3y3z1_nvb(pes_x3y3z1_vpc%dg)) then
 stop 'pes_x3y3z1_vread: dimension error'
endif
allocate (pes_x3y3z1_vcf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_x3y3z1_vcf
endif
close (iun)
return
END SUBROUTINE pes_x3y3z1_vread
