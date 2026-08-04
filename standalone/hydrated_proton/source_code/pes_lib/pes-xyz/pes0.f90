MODULE pes0
!..use and access
use inv, wp=>inv_wp
implicit none
private
public :: pes0_init, pes_getiun, pes_nmbrec, pes_dists, &
  pes_write0, pes_write
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
include 'pes0_init.f90'
include 'pes_getiun.f90'
include 'pes_nmbrec.f90'
include 'pes_dists.f90'
include 'pes_write0.f90'
include 'pes_write.f90'
END MODULE pes0
