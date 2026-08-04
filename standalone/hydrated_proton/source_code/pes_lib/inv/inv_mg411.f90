MODULE inv_mg411
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg411_prims, mg411_prib, mg411_isecs, mg411_secs, &
  mg411_base
!..data
integer, parameter, private :: nkk=3, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,1,1/)
integer, parameter, public :: &
  mg411_id=23, &
  mg411_nkk=nkk, mg411_nk=nk, mg411_nr=nr, mg411_ngrp=24, &
  mg411_nkj(0:nkk-1)=nkj, &
  mg411_dnpr(0:9) = (/ 0, 4, 4, 4, 3, 0, 0, 0, 0, 0 /), &
  mg411_npr(0:9) = (/ 0, 4, 8, 12, 15, 15, 15, 15, 15, 15 /), &
  mg411_dnpb(0:9) = (/ 1, 4, 14, 40, 104, 244, 540, 1124, 2239, 4276 /), &
  mg411_npb(0:9) = (/ 1, 5, 19, 59, 163, 407, 947, 2071, 4310, 8586 /), &
  mg411_dnsc(0:9) = (/ 1, 0, 3, 13, 32, 62, 129, 221, 335, 442 /), &
  mg411_nsc(0:9) = (/ 1, 1, 4, 17, 49, 111, 240, 461, 796, 1238 /), &
  mg411_dnb(0:9) = (/ 1, 4, 17, 65, 230, 736, 2197, 6093, 15864, &
    38960 /), &
  mg411_nb(0:9) = (/ 1, 5, 22, 87, 317, 1053, 3250, 9343, 25207, &
    64167 /)
!..procedures
CONTAINS
include 'mg411_prims.f90'
include 'mg411_prib.f90'
include 'mg411_isecs.f90'
include 'mg411_secs.f90'
include 'mg411_base.f90'
END MODULE inv_mg411
