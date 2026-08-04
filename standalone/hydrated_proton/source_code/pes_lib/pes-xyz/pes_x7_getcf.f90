SUBROUTINE pes_x7_getcf (nki, nkj, pc, cf)
! Callback routine for inv/cx_getcf
integer, intent (in) :: nki(0:), nkj(0:)
type (cx_t), intent (in) :: pc
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: iun
call pes_getiun (iun)
if (size(nki).ne.size(pes_x7_nki)) then
 stop 'pes_x7_getcf: bad dimension'
else if (any(nki.ne.pes_x7_nki)) then
 stop 'pes_x7_getcf: bad nki'
endif
if (all(nkj.eq.(/1/))) then
 call pes_x1_add (cf)
 call pes_write0 (iun, 'pcf-x1', pes_x1_cf)
else if (all(nkj.eq.(/2/))) then
 call pes_x2_add (pc, cf)
 call pes_write (iun, 'pcf-x2', pes_x2_pc, pes_x2_cf)
else if (all(nkj.eq.(/3/))) then
 call pes_x3_add (pc, cf)
 call pes_write (iun, 'pcf-x3', pes_x3_pc, pes_x3_cf)
else if (all(nkj.eq.(/4/))) then
 call pes_x4_add (pc, cf)
 call pes_write (iun, 'pcf-x4', pes_x4_pc, pes_x4_cf)
else if (all(nkj.eq.(/5/))) then
 call pes_x5_add (pc, cf)
 call pes_write (iun, 'pcf-x5', pes_x5_pc, pes_x5_cf)
else if (all(nkj.eq.(/6/))) then
 call pes_x6_add (pc, cf)
 call pes_write (iun, 'pcf-x6', pes_x6_pc, pes_x6_cf)
else if (all(nkj.eq.(/7/))) then
 call pes_x7_add (pc, cf)
 call pes_write (iun, 'pcf-x7', pes_x7_pc, pes_x7_cf)
else
 stop 'pes_x7_getcf: bad nkj'
end if
return
END SUBROUTINE pes_x7_getcf
