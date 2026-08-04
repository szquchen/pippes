MODULE pes_h
!..use and access
use pes, wp=>pes_wp
private
public :: h_init, h_pot
!..data
save
integer, parameter :: &
  nki(0:1)=(/3,1/), nk=4, iord(0:nk-1)=(/1,2,3,4/)
real (kind=wp) :: x1_cf, y1_cf, z1_cf
type (cx_t) :: &
  x3y1_pc   = cx_null, x3y1_vpc   = cx_null
real (kind=wp) :: x1_vcf(0:0), y1_vcf(0:0), z1_vcf(0:0)
real (kind=wp), allocatable ::   &
  x3y1_cf(:)   , x3y1_vcf(:)
!..procedures
CONTAINS
SUBROUTINE h_init (dirname)
character (len=*), intent (in) :: dirname
!------------------------------------------------------------------------
integer :: iun, nb
logical :: b0
character (len=255) :: chd
call pes_getiun (iun)
b0 = dirname(len_trim(dirname):len_trim(dirname)).eq.'/'
if (b0) then
 chd = dirname
else
 chd = trim(dirname)//'/'
endif
! - - -
open (iun, status='old', file=trim(chd)//'pcf-x1.dat')
read (iun,*) x1_cf
close (iun)
open (iun, status='old', file=trim(chd)//'pcf-y1.dat')
read (iun,*) y1_cf
close (iun)
! - - -
open (iun, status='old', file=trim(chd)//'pcf-x3y1.dat')
read (iun,*) x3y1_pc
read (iun,*) nb
if (nb.ne.pes_x3y1_nb(x3y1_pc%dg)) then
 stop 'h3ot_init: x3y1 dimension error'
endif
allocate (x3y1_cf(0:nb-1))
if (1.le.nb) then
 read (iun,*) x3y1_cf
endif
close (iun)
! - - -
return
END SUBROUTINE h_init

FUNCTION h_pot (xn)
implicit none
real (kind=wp), dimension(:,:), intent (in) :: xn
real (kind=wp) :: h_pot
!-----------------------------------------------------------------------
real (kind=wp) :: xn0(0:2,0:nk-1), r(0:nk-1,0:nk-1)

call pes_dists (xn, r)

h_pot = &
  !x1_cf*nki(0)                          + &
  !y1_cf*nki(1)                          + &
  !z1_cf*nki(2)                          + &
  cx_f31 (nki, r, x3y1_pc,   x3y1_cf)
return
END FUNCTION h_pot
END MODULE pes_h
