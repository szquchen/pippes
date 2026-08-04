MODULE inv_mg432
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg432_prims, mg432_prib, mg432_isecs, mg432_secs, &
  mg432_base
!..data
integer, parameter, private :: nkk=3, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,3,2/)
integer, parameter, public :: &
  mg432_id=77, &
  mg432_nkk=nkk, mg432_nk=nk, mg432_nr=nr, mg432_ngrp=288, &
  mg432_nkj(0:nkk-1)=nkj, &
  mg432_dnpr(0:9) = (/ 0, 6, 12, 8, 6, 0, 3, 0, 0, 0 /), &
  mg432_npr(0:9) = (/ 0, 6, 18, 26, 32, 32, 35, 35, 35, 35 /), &
  mg432_dnpb(0:9) = (/ 1, 6, 33, 136, 510, 1692, 5237, 15066, 41136, &
    106742 /), &
  mg432_npb(0:9) = (/ 1, 7, 40, 176, 686, 2378, 7615, 22681, 63817, &
    170559 /), &
  mg432_dnsc(0:9) = (/ 1, 0, 7, 56, 239, 1083, 4926, 19904, 73665, &
    250996 /), &
  mg432_nsc(0:9) = (/ 1, 1, 8, 64, 303, 1386, 6312, 26216, 99881, &
    350877 /), &
  mg432_dnb(0:9) = (/ 1, 6, 40, 234, 1316, 7009, 35734, 173173, 797372, &
    3481948 /), &
  mg432_nb(0:9) = (/ 1, 7, 47, 281, 1597, 8606, 44340, 217513, 1014885, &
    4496833 /)
!..procedures
CONTAINS
include 'mg432_prims.f90'
include 'mg432_prib.f90'
include 'mg432_isecs.f90'
include 'mg432_secs.f90'
include 'mg432_base.f90'
END MODULE inv_mg432
