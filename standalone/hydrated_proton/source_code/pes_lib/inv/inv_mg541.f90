MODULE inv_mg541
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg541_prims, mg541_prib, mg541_isecs, mg541_secs, &
  mg541_base
!..data
integer, parameter, private :: nkk=3, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,4,1/)
integer, parameter, public :: &
  mg541_id=106, &
  mg541_nkk=nkk, mg541_nk=nk, mg541_nr=nr, mg541_ngrp=120*24, &
  mg541_nkj(0:nkk-1)=nkj, &
  mg541_dnpr(0:9) = (/ 0, 5, 9, 11, 10, 4, 2, 0, 0, 0 /), &
  mg541_npr(0:9) = (/ 0, 5, 14, 25, 35, 39, 41, 41, 41, 41 /), &
  mg541_dnpb(0:9) = (/ 1, 5, 24, 91, 315, 984, 2888, 7960, &
    20932, 52668 /), &
  mg541_npb(0:9) = (/ 1, 6, 30, 121, 436, 1420, 4308, 12268, &
    33200, 85868 /), &
  mg541_dnsc(0:9) = (/ 1, 0, 6, 44, 218, 1066, 5351, 25766, &
    118341, 514608 /), &
  mg541_nsc(0:9) = (/ 1, 1, 7, 51, 269, 1335, 6686, 32452, &
    150793, 665401 /), &
  mg541_dnb(0:9) = (/ 1, 5, 30, 165, 897, 4742, 24695, 125667, &
    622827, 2989440 /), &
  mg541_nb(0:9) = (/ 1, 6, 36, 201, 1098, 5840, 30535, 156202, &
    779029, 3768469 /)
!..procedures
CONTAINS
include 'mg541_prims.f90'
include 'mg541_prib.f90'
include 'mg541_isecs.f90'
include 'mg541_secs.f90'
include 'mg541_base.f90'
END MODULE inv_mg541
