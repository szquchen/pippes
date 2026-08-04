MODULE inv_sym3
!..use and access
use inv_wp
implicit none
private
public :: sym3_gens, sym3_prims, sym3_secs, sym3_base
!..data
integer, parameter, public :: &
  sym3_nr=3, sym3_ngrp=6, sym3_ngen=2, &
  sym3_dnpr(0:9) = (/ 0, 1, 1, 1, 0, 0, 0, 0, 0, 0 /), &
  sym3_npr(0:9) = (/ 0, 1, 2, 3, 3, 3, 3, 3, 3, 3 /), &
  sym3_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  sym3_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  sym3_dnb(0:9) = (/ 1, 1, 2, 3, 4, 5, 7, 8, 10, 12 /), &
  sym3_nb(0:9) = (/ 1, 2, 4, 7, 11, 16, 23, 31, 41, 53 /)
!..procedures
CONTAINS
include 'sym3_gens.f90'
include 'sym3_prims.f90'
include 'sym3_secs.f90'
include 'sym3_base.f90'
END MODULE inv_sym3
