MODULE pes0_t
!..use and access
use inv_t
implicit none
private
public :: pes0_init, pes_getiun, pes_dists!, pes_write0, pes_write
!..data
save
integer, parameter, public :: pes0_wp=wp
real (kind=wp), parameter, public :: &
  pes_hartree=1.0_wp, pes_joule=pes_hartree/4.359744e-18_wp, &
  pes_wavenmb=pes_hartree/219474.63137_wp, &
  pes_bohr=1.0_wp, pes_angstrom=pes_bohr/0.529177249_wp, &
  pes_elcharge=1.0_wp, pes_coulomb=pes_elcharge/1.60217653e-19_wp
character (len=255), public :: pes_dir='./'
character (len=16), public :: pcf_suffix='.out'
!..procedures
CONTAINS
SUBROUTINE pes0_init (dir, suffix)
! Initialize module data in pes0
character (len=*), intent (in), optional :: dir, suffix
!-----------------------------------------------------------------------
logical :: b0
if (present(dir)) then
 b0 = dir(len_trim(dir):len_trim(dir)).eq.'/'
 if (b0) then
!  write (*,*) 'Principal data directory: ', dir(1:len_trim(dir))
  pes_dir = dir
 else
!  write (*,*) 'Principal data directory: ', dir(1:len_trim(dir))//'/'
  pes_dir = trim(dir)//'/'
 endif
endif
if (present(suffix)) then
 write (*,*) 'pcf output suffix: ', suffix(1:len_trim(suffix))
 pcf_suffix = suffix
endif
return
END SUBROUTINE pes0_init

SUBROUTINE pes_getiun (iun)
! Obtain a free unit number
integer, intent (out) :: iun
!-----------------------------------------------------------------------
integer :: k
logical :: b
k = 20
inquire (unit=k, opened=b)
do while (b.and.k.lt.100)
 k = k+1
 inquire (unit=k, opened=b)
enddo
if (.not.b) then
 iun = k
else
 stop 'pes_getiun: no free unit'
endif
return
END SUBROUTINE pes_getiun

SUBROUTINE pes_dists (xn, d)
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp), intent (out) :: d(0:,0:)
!-----------------------------------------------------------------------
integer :: i, j, n
if (size(d,1).ne.size(xn,2).or.size(d,2).ne.size(xn,2)) then
 stop 'pes_dists: bad dimensions'
endif
n = size(xn,2)
do j = 0, n-1
 do i = 0, j-1
  d(i,j) = sqrt(sum((xn(:,j)-xn(:,i))**2))
  d(j,i) = d(i,j)
 enddo
 d(j,j) = 0
enddo
return
END SUBROUTINE pes_dists

END MODULE pes0_t
