MODULE inv_mg631
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg631_prims, mg631_prib, mg631_isecs, mg631_secs, &
  mg631_base
!..data
integer, parameter, private :: nkk=3, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,3,1/)
integer, parameter, public :: &
  mg631_id=105, &
  mg631_nkk=nkk, mg631_nk=nk, mg631_nr=nr, mg631_ngrp=720*6, &
  mg631_nkj(0:nkk-1)=nkj, &
  mg631_dnpr(0:9) = (/ 0, 5, 8, 10, 8, 6, 5, 0, 0, 0 /), &
  mg631_npr(0:9) = (/ 0, 5, 13, 23, 31, 37, 42, 42, 42, 42 /), &
  mg631_dnpb(0:9) = (/ 1, 5, 23, 85, 284, 862, 2454, 6576, &
    16819, 41223 /), &
  mg631_npb(0:9) = (/ 1, 6, 29, 114, 398, 1260, 3714, 10290, &
    27109, 68332 /), &
  mg631_dnsc(0:9) = (/ 1, 0, 6, 38, 172, 837, 4104, 19240, &
    87305, 377449 /), &
  mg631_nsc(0:9) = (/ 1, 1, 7, 45, 217, 1054, 5158, 24398, &
    111703, 489152 /), &
  mg631_dnb(0:9) = (/ 1, 5, 29, 153, 784, 3943, 19633, 96171, &
    462189, 2165237 /), &
  mg631_nb(0:9) = (/ 1, 6, 35, 188, 972, 4915, 24548, 120719, &
    582908, 2748145 /)
!..procedures
CONTAINS
include 'mg631_prims.f90'
include 'mg631_prib.f90'
include 'mg631_isecs.f90'
include 'mg631_secs.f90'
include 'mg631_base.f90'
END MODULE inv_mg631
