MODULE dms_h
!..use and access
use pes, wp=>pes_wp
use px
private
public :: h_dip_init, h_dip
!..data
save
integer, parameter :: &
  nki(0:1)=(/3,1/), nk=4, iord(0:nk-1)=(/1,2,3,4/)
real (kind=wp) :: x1_cf, y1_cf, z1_cf
type (cx_t) :: &
!       x1y1_pc = cx_null,       x1y1_vpc = cx_null, &
!         x2_pc = cx_null,         x2_vpc = cx_null, &
       x3y1_pc = cx_null,       x3y1_vpc = cx_null 
real (kind=wp) :: x1_vcf(0:0), y1_vcf(0:0), z1_vcf(0:0)
real (kind=wp), allocatable ::   &
!           x1y1_cf(:)   ,            x1y1_vcf(:)  , &
!             x2_cf(:)   ,              x2_vcf(:)  , &
           x3y1_cf(:)   ,            x3y1_vcf(:)
!..procedures
CONTAINS
SUBROUTINE h_dip_init (dirname)
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
write (*,*) 'Principal data directory: ', chd(1:len_trim(chd))
! - - -
write (*,*) ' reading vpcf-x1.dat'
open (iun, status='old', file=trim(chd)//'vpcf-x1.dat')
read (iun,*) x1_vcf
close (iun)
! - - -
write (*,*) ' reading vpcf-y1.dat'
open (iun, status='old', file=trim(chd)//'vpcf-y1.dat')
read (iun,*) y1_vcf
close (iun)
! - - -
write (*,*) ' reading vpcf-x3y1.dat'
open (iun, status='old', file=trim(chd)//'vpcf-x3y1.dat')
read (iun,*) x3y1_vpc
read (iun,*) nb
allocate (x3y1_vcf(0:nb-1))
if (1.le.nb) then
 read (iun,*) x3y1_vcf
endif
close (iun)
END SUBROUTINE h_dip_init

FUNCTION h_dip (xn) RESULT(q)
implicit none
real (kind=wp), dimension(:,:), intent (in) :: xn
real (kind=wp) :: q(4)
!-----------------------------------------------------------------------
real (kind=wp) :: xn0(0:2,0:nk-1), r(0:nk-1,0:nk-1)

call pes_dists (xn, r)

q = cxv_f1(nki,x1_vcf)+&
    cxv_f01(nki,y1_vcf)+&
    cxv_f31 (nki, r, x3y1_vpc, x3y1_vcf)   
return
END FUNCTION h_dip
 
END MODULE dms_h
