SUBROUTINE pes_mol_read (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) mol_pc
read (iun,*) nb
if (nb.ne.mol_nb(mol_pc%dg)) then
 stop 'pes_mol_read: dimension error'
endif
allocate (mol_cf(0:nb-1))
if (1.le.nb) then
 read (iun,*) mol_cf
endif
close (iun)
return
END SUBROUTINE pes_mol_read
