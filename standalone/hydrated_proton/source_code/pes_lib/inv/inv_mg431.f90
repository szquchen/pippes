MODULE inv_mg431
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg431_prims, mg431_prib, mg431_isecs, mg431_secs, &
  mg431_base
!..data
integer, parameter, private :: nkk=3, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,3,1/)
integer, parameter, public :: &
  mg431_id=52, &
  mg431_nkk=nkk, mg431_nk=nk, mg431_nr=nr, mg431_ngrp=144, &
  mg431_nkj(0:nkk-1)=nkj, &
  mg431_dnpr(0:9) = (/ 0, 5, 8, 8, 5, 0, 1, 0, 0, 0 /), &
  mg431_npr(0:9) = (/ 0, 5, 13, 21, 26, 26, 27, 27, 27, 27 /), &
  mg431_dnpb(0:9) = (/ 1, 5, 23, 83, 271, 795, 2182, 5606, 13709, 32009 /), &
  mg431_npb(0:9) = (/ 1, 6, 29, 112, 383, 1178, 3360, 8966, 22675, 54684 /), &
  mg431_dnsc(0:9) = (/ 1, 0, 6, 36, 135, 524, 1944, 6351, 19068, 52086 /), &
  mg431_nsc(0:9) = (/ 1, 1, 7, 43, 178, 702, 2646, 8997, 28065, 80151 /), &
  mg431_dnb(0:9) = (/ 1, 5, 29, 149, 724, 3320, 14465, 59460, 231033, &
    848377 /), &
  mg431_nb(0:9) = (/ 1, 6, 35, 184, 908, 4228, 18693, 78153, 309186, &
    1157563 /)
!..procedures
CONTAINS
include 'mg431_prims.f90'
include 'mg431_prib.f90'
include 'mg431_isecs.f90'
include 'mg431_secs.f90'
include 'mg431_base.f90'
END MODULE inv_mg431
