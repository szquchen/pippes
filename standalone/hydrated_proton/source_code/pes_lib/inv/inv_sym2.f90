MODULE inv_sym2
!..use and access
use inv_wp
implicit none
private
public :: sym2_gens, sym2_prims, sym2_secs, sym2_base
!..data
integer, parameter, public :: &
  sym2_nr=2, sym2_ngrp=2, sym2_ngen=1, &
  sym2_dnpr(0:9) = (/ 0, 1, 1, 0, 0, 0, 0, 0, 0, 0 /), &
  sym2_npr(0:9) = (/ 0, 1, 2, 2, 2, 2, 2, 2, 2, 2 /), &
  sym2_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  sym2_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  sym2_dnb(0:9) = (/ 1, 1, 2, 2, 3, 3, 4, 4, 5, 5 /), &
  sym2_nb(0:9) = (/ 1, 2, 4, 6, 9, 12, 16, 20, 25, 30 /)
!..procedures
CONTAINS
include 'sym2_gens.f90'
include 'sym2_prims.f90'
include 'sym2_secs.f90'
include 'sym2_base.f90'
END MODULE inv_sym2
