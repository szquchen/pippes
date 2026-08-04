MODULE inv_mg411111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg411111_prims, mg411111_prib, mg411111_isecs, mg411111_secs, &
  mg411111_base
!..data
integer, parameter, private :: nkk=6, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,1,1,1,1,1/)
integer, parameter, public :: &
  mg411111_id=90, &
  mg411111_nkk=nkk, mg411111_nk=nk, mg411111_nr=nr, mg411111_ngrp=24, &
  mg411111_nkj(0:nkk-1)=nkj, &
  mg411111_dnpr(0:9) = (/ 0, 16, 7, 7, 6, 0, 0, 0, 0, 0 /), &
  mg411111_npr(0:9) = (/ 0, 16, 23, 30, 36, 36, 36, 36, 36, 36 /), &
  mg411111_dnpb(0:9) = (/ 1, 16, 143, 935, 4974, 22761, 92670, 343314, &
    1175805, 3766738 /), &
  mg411111_npb(0:9) = (/ 1, 17, 160, 1095, 6069, 28830, 121500, 464814, &
    1640619, 5407357 /), &
  mg411111_dnsc(0:9) = (/ 1, 0, 15, 71, 331, 1261, 4552, 13757, &
    37193, 89279 /), &
  mg411111_nsc(0:9) = (/ 1, 1, 16, 87, 418, 1679, 6231, 19988, &
    57181, 146460 /), &
  mg411111_dnb(0:9) = (/ 1, 16, 158, 1246, 8586, 53496, 305726, 1614280, &
    7915556, 36209861 /), &
  mg411111_nb(0:9) = (/ 1, 17, 175, 1421, 10007, 63503, 369229, 1983509, &
    9899065, 46108926 /)
!..procedures
CONTAINS
include 'mg411111_prims.f90'
include 'mg411111_prib.f90'
include 'mg411111_isecs.f90'
include 'mg411111_secs.f90'
include 'mg411111_base.f90'
END MODULE inv_mg411111
