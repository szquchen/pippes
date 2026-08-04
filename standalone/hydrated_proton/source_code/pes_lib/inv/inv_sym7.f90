MODULE inv_sym7
!..use and access
use inv_wp
implicit none
private
public :: sym7_gens, sym7_prims, sym7_secs, sym7_base
!..data
integer, parameter, public :: &
  sym7_nr=7, sym7_ngrp=5040, sym7_ngen=2, &
  sym7_dnpr(0:9) = (/ 0, 1, 1, 1, 1, 1, 1, 1, 0, 0 /), &
  sym7_npr(0:9) = (/ 0, 1, 2, 3, 4, 5, 6, 7, 7, 7 /), &
  sym7_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  sym7_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  sym7_dnb(0:9) = (/ 1, 1, 2, 3, 5, 7, 11, 15, 21, 28 /), &
  sym7_nb(0:9) = (/ 1, 2, 4, 7, 12, 19, 30, 45, 66, 94 /)
!..procedures
CONTAINS
include 'sym7_gens.f90'
include 'sym7_prims.f90'
include 'sym7_secs.f90'
include 'sym7_base.f90'
END MODULE inv_sym7
