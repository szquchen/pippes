MODULE inv_sym5
!..use and access
use inv_wp
implicit none
private
public :: sym5_gens, sym5_prims, sym5_secs, sym5_base
!..data
integer, parameter, public :: &
  sym5_nr=5, sym5_ngrp=120, sym5_ngen=2, &
  sym5_dnpr(0:9) = (/ 0, 1, 1, 1, 1, 1, 0, 0, 0, 0 /), &
  sym5_npr(0:9) = (/ 0, 1, 2, 3, 4, 5, 5, 5, 5, 5 /), &
  sym5_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  sym5_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  sym5_dnb(0:9) = (/ 1, 1, 2, 3, 5, 7, 10, 13, 18, 23 /), &
  sym5_nb(0:9) = (/ 1, 2, 4, 7, 12, 19, 29, 42, 60, 83 /)
!..procedures
CONTAINS
include 'sym5_gens.f90'
include 'sym5_prims.f90'
include 'sym5_secs.f90'
include 'sym5_base.f90'
END MODULE inv_sym5
