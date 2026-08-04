MODULE inv_mg6111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg6111_prims, mg6111_prib, mg6111_isecs, mg6111_secs, &
  mg6111_base
!..data
integer, parameter, private :: nkk=4, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,1,1,1/)
integer, parameter, public :: &
  mg6111_id=79, &
  mg6111_nkk=nkk, mg6111_nk=nk, mg6111_nr=nr, mg6111_ngrp=720, &
  mg6111_nkj(0:nkk-1)=nkj, &
  mg6111_dnpr(0:9) = (/ 0, 7, 5, 7, 6, 6, 5, 0, 0, 0 /), &
  mg6111_npr(0:9) = (/ 0, 7, 12, 19, 25, 31, 36, 36, 36, 36 /), &
  mg6111_dnpb(0:9) = (/ 1, 7, 33, 126, 420, 1266, 3535, 9271, 23082, &
    54969 /), &
  mg6111_npb(0:9) = (/ 1, 8, 41, 167, 587, 1853, 5388, 14659, 37741, &
    92710 /), &
  mg6111_dnsc(0:9) = (/ 1, 0, 6, 29, 131, 575, 2460, 9764, 36949, &
    130295 /), &
  mg6111_nsc(0:9) = (/ 1, 1, 7, 36, 167, 742, 3202, 12966, 49915, &
    180210 /), &
  mg6111_dnb(0:9) = (/ 1, 7, 39, 197, 952, 4471, 20517, 91512, 394953, &
    1641566 /), &
  mg6111_nb(0:9) = (/ 1, 8, 47, 244, 1196, 5667, 26184, 117696, 512649, &
    2154215 /)
!..procedures
CONTAINS
include 'mg6111_prims.f90'
include 'mg6111_prib.f90'
include 'mg6111_isecs.f90'
include 'mg6111_secs.f90'
include 'mg6111_base.f90'
END MODULE inv_mg6111
