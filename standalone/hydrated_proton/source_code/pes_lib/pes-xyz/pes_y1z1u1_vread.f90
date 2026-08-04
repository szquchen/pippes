SUBROUTINE pes_y1z1u1_vread (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_y1z1u1_vpc
read (iun,*) nb
if (nb.ne.pes_y1z1u1_nvb(pes_y1z1u1_vpc%dg)) then
 stop 'pes_y1z1u1_vread: dimension error'
endif
allocate (pes_y1z1u1_vcf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_y1z1u1_vcf
endif
close (iun)
return
END SUBROUTINE pes_y1z1u1_vread
