module inv_mg42_t
!..use and access
use inv_share
implicit none
private
public :: mg42_gens, mg42_prims, mg42_secs, mg42_base
!..types
!..data
integer, parameter, public :: &
  mg42_nr=15, mg42_ngrp=48, mg42_ngen=3, &
  mg42_dvp(0:9) = (/ 0, 3, 5, 3, 3, 0, 1, 0, 0, 0 /), &
  mg42_ivp(0:9) = (/ 0, 3, 8, 11, 14, 14, 15, 15, 15, 15 /), &
  mg42_dvs(0:9) = (/ 1, 0, 1, 10, 24, 47, 101, 195, 328, 489 /), &
  mg42_ivs(0:9) = (/ 1, 1, 2, 12, 36, 83, 184, 379, 707, 1196 /), &
  mg42_dvb(0:9) = (/ 1, 3, 12, 41, 137, 415, 1201, 3246, 8319, 20180 /), &
  mg42_ivb(0:9) = (/ 1, 4, 16, 57, 194, 609, 1810, 5056, 13375, 33555 /)
!..procedures
contains
include "mg42_gens.f90"
include "mg42_prims.f90"
include "mg42_secs.f90"
include "mg42_base.f90"
end module inv_mg42_t
