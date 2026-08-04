MODULE inv_mg42
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg42_prims, mg42_prib, mg42_isecs, mg42_secs, &
  mg42_base
!..data
integer, parameter, private :: nkk=2, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,2/)
integer, parameter, public :: &
  mg42_id=21, &
  mg42_nkk=nkk, mg42_nk=nk, mg42_nr=nr, mg42_ngrp=48, &
  mg42_nkj(0:nkk-1)=nkj, &
  mg42_dnpr(0:9) = (/ 0, 3, 5, 3, 3, 0, 1, 0, 0, 0 /), &
  mg42_npr(0:9) = (/ 0, 3, 8, 11, 14, 14, 15, 15, 15, 15 /), &
  mg42_dnpb(0:9) = (/ 1, 3, 11, 28, 72, 158, 343, 681, 1328, 2447 /), &
  mg42_npb(0:9) = (/ 1, 4, 15, 43, 115, 273, 616, 1297, 2625, 5072 /), &
  mg42_dnsc(0:9) = (/ 1, 0, 1, 10, 24, 47, 101, 195, 328, 489 /), &
  mg42_nsc(0:9) = (/ 1, 1, 2, 12, 36, 83, 184, 379, 707, 1196 /), &
  mg42_dnb(0:9) = (/ 1, 3, 12, 41, 137, 415, 1201, 3246, 8319, 20180 /), &
  mg42_nb(0:9) = (/ 1, 4, 16, 57, 194, 609, 1810, 5056, 13375, 33555 /)
!..procedures
CONTAINS
include 'mg42_prims.f90'
include 'mg42_prib.f90'
include 'mg42_isecs.f90'
include 'mg42_secs.f90'
include 'mg42_base.f90'
END MODULE inv_mg42
