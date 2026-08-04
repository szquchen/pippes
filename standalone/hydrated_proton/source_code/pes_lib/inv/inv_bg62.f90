MODULE inv_bg62
!..use and access
use inv_wp
implicit none
private
public :: bg62_gens, bg62_prims, bg62_secs, bg62_base
!..data
integer, parameter, public :: &
  bg62_nr=12, bg62_ngrp=1440, bg62_ngen=3, &
  bg62_dnpr(0:9) = (/ 0, 1, 3, 2, 2, 1, 2, 0, 0, 0 /), &
  bg62_npr(0:9) = (/ 0, 1, 4, 6, 8, 9, 11, 11, 11, 11 /), &
  bg62_dnsc(0:9) = (/ 1, 0, 0, 1, 4, 6, 10, 12, 18, 25 /), &
  bg62_nsc(0:9) = (/ 1, 1, 1, 2, 6, 12, 22, 34, 52, 77 /), &
  bg62_dnb(0:9) = (/ 1, 1, 4, 7, 19, 35, 80, 145, 291, 517 /), &
  bg62_nb(0:9) = (/ 1, 2, 6, 13, 32, 67, 147, 292, 583, 1100 /)
!..procedures
CONTAINS
include 'bg62_gens.f90'
include 'bg62_prims.f90'
include 'bg62_secs.f90'
include 'bg62_base.f90'
END MODULE inv_bg62
