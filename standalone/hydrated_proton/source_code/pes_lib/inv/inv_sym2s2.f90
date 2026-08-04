MODULE inv_sym2s2
!..use and access
use inv_wp
implicit none
private
public :: sym2s2_gens, sym2s2_prims, sym2s2_secs, sym2s2_base
!..data
integer, parameter, public :: &
  sym2s2_nr=4, sym2s2_ngrp=4, sym2s2_ngen=2, &
  sym2s2_dnpr(0:9) = (/ 0, 2, 2, 0, 0, 0, 0, 0, 0, 0 /), &
  sym2s2_npr(0:9) = (/ 0, 2, 4, 4, 4, 4, 4, 4, 4, 4 /), &
  sym2s2_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  sym2s2_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  sym2s2_dnb(0:9) = (/ 1, 2, 5, 8, 14, 20, 30, 40, 55, 70 /), &
  sym2s2_nb(0:9) = (/ 1, 3, 8, 16, 30, 50, 80, 120, 175, 245 /)
!..procedures
CONTAINS
include 'sym2s2_gens.f90'
include 'sym2s2_prims.f90'
include 'sym2s2_secs.f90'
include 'sym2s2_base.f90'
END MODULE inv_sym2s2
