SUBROUTINE pes_mol_vread (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) mol_vpc
read (iun,*) nb
if (nb.ne.mol_nvb(mol_vpc%dg)) then
 stop 'pes_mol_vread: dimension error'
endif
allocate (mol_vcf(0:nb-1))
if (1.le.nb) then
 read (iun,*) mol_vcf
endif
close (iun)
return
END SUBROUTINE pes_mol_vread
