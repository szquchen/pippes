MODULE inv_sym4
!..use and access
use inv_wp
implicit none
private
public :: sym4_gens, sym4_prims, sym4_secs, sym4_base
!..data
integer, parameter, public :: &
  sym4_nr=4, sym4_ngrp=24, sym4_ngen=2, &
  sym4_dnpr(0:9) = (/ 0, 1, 1, 1, 1, 0, 0, 0, 0, 0 /), &
  sym4_npr(0:9) = (/ 0, 1, 2, 3, 4, 4, 4, 4, 4, 4 /), &
  sym4_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  sym4_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  sym4_dnb(0:9) = (/ 1, 1, 2, 3, 5, 6, 9, 11, 15, 18 /), &
  sym4_nb(0:9) = (/ 1, 2, 4, 7, 12, 18, 27, 38, 53, 71 /)
!..procedures
CONTAINS
include 'sym4_gens.f90'
include 'sym4_prims.f90'
include 'sym4_secs.f90'
include 'sym4_base.f90'
END MODULE inv_sym4
