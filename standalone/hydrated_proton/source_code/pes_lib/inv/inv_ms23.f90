MODULE inv_ms23
!..use and access
use inv_wp
implicit none
private
public :: ms23_gens, ms23_prims, ms23_secs, ms23_base
!..data
integer, parameter, public :: &
  ms23_nr=6, ms23_ngrp=2, ms23_ngen=1, &
  ms23_dnpr(0:9) = (/ 0, 3, 3, 0, 0, 0, 0, 0, 0, 0 /), &
  ms23_npr(0:9) = (/ 0, 3, 6, 6, 6, 6, 6, 6, 6, 6 /), &
  ms23_dnsc(0:9) = (/ 1, 0, 3, 0, 0, 0, 0, 0, 0, 0 /), &
  ms23_nsc(0:9) = (/ 1, 1, 4, 4, 4, 4, 4, 4, 4, 4 /), &
  ms23_dnb(0:9) = (/ 1, 3, 12, 28, 66, 126, 236, 396, 651, 1001 /), &
  ms23_nb(0:9) = (/ 1, 4, 16, 44, 110, 236, 472, 868, 1519, 2520 /)
!..procedures
CONTAINS
include 'ms23_gens.f90'
include 'ms23_prims.f90'
include 'ms23_secs.f90'
include 'ms23_base.f90'
END MODULE inv_ms23
