MODULE inv_sym6
!..use and access
use inv_wp
implicit none
private
public :: sym6_gens, sym6_prims, sym6_secs, sym6_base
!..data
integer, parameter, public :: &
  sym6_nr=6, sym6_ngrp=720, sym6_ngen=2, &
  sym6_dnpr(0:9) = (/ 0, 1, 1, 1, 1, 1, 1, 0, 0, 0 /), &
  sym6_npr(0:9) = (/ 0, 1, 2, 3, 4, 5, 6, 6, 6, 6 /), &
  sym6_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  sym6_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  sym6_dnb(0:9) = (/ 1, 1, 2, 3, 5, 7, 11, 14, 20, 26 /), &
  sym6_nb(0:9) = (/ 1, 2, 4, 7, 12, 19, 30, 44, 64, 90 /)
!..procedures
CONTAINS
include 'sym6_gens.f90'
include 'sym6_prims.f90'
include 'sym6_secs.f90'
include 'sym6_base.f90'
END MODULE inv_sym6
