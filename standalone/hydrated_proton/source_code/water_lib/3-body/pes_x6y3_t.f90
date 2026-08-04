MODULE pes_x6y3_t
!..use and access
use inv_t
use pes0_t
private
public :: pes_x6y3_read, pes_x6y3_pot!, pes_x6y3_add
!..data
save
integer, parameter, public :: &
  pes_x6y3_nki(0:1)=(/6,3/), pes_x6y3_nk=9, &
  pes_x6y3_nb(-1:size(cx_nb63(-1:))-2)=cx_nb63(-1:)
character (len=*), parameter, public :: &
  pes_x6y3_sysall='x1 y1 x2 x1y1 y2 x3 x2y1 x1y2 y3 x4 x3y1 x2y2 x1y3 '// &
    'x5 x4y1 x3y2 x2y3 x6 x5y1 x4y2 x3y3 x6y1 x5y2 x4y3 x5y2 x4y3 '// &
    'x6y2 x5y3 x6y3'
type (cx_t), public :: &
  pes_x6y3_pc = cx_null, &
  pes_x6y3_vpc = cx_null
real (kind=wp), allocatable, public :: &
  pes_x6y3_cf(:), pes_x6y3_vcf(:)
!..procedures
CONTAINS
SUBROUTINE pes_x6y3_read (iun, fn)
integer, intent (in) :: iun
character (len=*), intent (in) :: fn
!-----------------------------------------------------------------------
integer :: nb
open (iun, status='old', file=fn)
read (iun,*) pes_x6y3_pc
read (iun,*) nb
if (nb.ne.pes_x6y3_nb(pes_x6y3_pc%dg)) then
 stop 'pes_x6y3_read: dimension error'
endif
allocate (pes_x6y3_cf(0:nb-1))
if (1.le.nb) then
 read (iun,*) pes_x6y3_cf
endif
close (iun)
call mg63_msecs()
return
END SUBROUTINE pes_x6y3_read

FUNCTION pes_x6y3_pot (xn) RESULT (f)
! Potential for generic X6Y3
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x6y3_nk-1,0:pes_x6y3_nk-1)
integer, parameter :: nki(0:1)=pes_x6y3_nki
call pes_dists (xn, r)
f = cx_f63(nki,r,pes_x6y3_pc,pes_x6y3_cf)
return
END FUNCTION pes_x6y3_pot

END MODULE pes_x6y3_t
