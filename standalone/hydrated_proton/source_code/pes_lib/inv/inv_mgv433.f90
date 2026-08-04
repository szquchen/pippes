MODULE inv_mgv433
!..use and access
use inv_wp
use inv_mg433
use inv_mg4321
use inv_mg3331
implicit none
private
public :: mgv433_base0, mgv433_base1, mgv433_base2, mgv433_base
!..data
integer, parameter, private :: &
  nkk=mg433_nkk, nk=mg433_nk, nr=mg433_nr, &
  nkj(0:nkk-1)=mg433_nkj
integer, parameter, public :: &
  mgv433_nb0(0:9) = mg3331_nb, &
  mgv433_nb1(0:9) = mg4321_nb, &
  mgv433_nb2(0:9) = mg4321_nb, &
  mgv433_nb(0:9) = mgv433_nb0+mgv433_nb1+mgv433_nb2, &
  mgv433_iord(0:nk-1,0:nk-1) = reshape( &
    (/1,2,3,4,5,6,7,8,9,0, &
      0,2,3,4,5,6,7,8,9,1, &
      0,1,3,4,5,6,7,8,9,2, &
      0,1,2,4,5,6,7,8,9,3, &
      0,1,2,3,7,8,9,5,6,4, &
      0,1,2,3,7,8,9,4,6,5, &
      0,1,2,3,7,8,9,4,5,6, &
      0,1,2,3,4,5,6,8,9,7, &
      0,1,2,3,4,5,6,7,9,8, &
      0,1,2,3,4,5,6,7,8,9/), (/nk,nk/))
!..procedures
CONTAINS
include 'mgv433_base0.f90'
include 'mgv433_base1.f90'
include 'mgv433_base2.f90'
include 'mgv433_base.f90'
END MODULE inv_mgv433
