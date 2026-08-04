SUBROUTINE pes_z2_getcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_z2_nki)) then
 stop 'pes_z2_getcf: bad dimension'
else if (any(nki.ne.pes_z2_nki)) then
 stop 'pes_z2_getcf: bad nki'
endif
if (all(nkj.eq.(/1/))) then
 call pes_z1_add (cf)
 call pes_write0 (iun, 'pcf-z1', pes_z1_cf)
else if (all(nkj.eq.(/2/))) then
 call pes_z2_add (pc, cf)
 call pes_write (iun, 'pcf-z2', pes_z2_pc, pes_z2_cf)
else
 stop 'pes_z2_getcf: bad nkj'
end if
return
END SUBROUTINE pes_z2_getcf
