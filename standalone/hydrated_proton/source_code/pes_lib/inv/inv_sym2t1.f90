MODULE inv_sym2t1
!..use and access
use inv_wp
implicit none
private
public :: sym2t1_gens, sym2t1_prims, sym2t1_secs, sym2t1_base
!..data
integer, parameter, public :: &
  sym2t1_nr=3, sym2t1_ngrp=2, sym2t1_ngen=1, &
  sym2t1_dnpr(0:9) = (/ 0, 2, 1, 0, 0, 0, 0, 0, 0, 0 /), &
  sym2t1_npr(0:9) = (/ 0, 2, 3, 3, 3, 3, 3, 3, 3, 3 /), &
  sym2t1_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  sym2t1_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  sym2t1_dnb(0:9) = (/ 1, 2, 4, 6, 9, 12, 16, 20, 25, 30 /), &
  sym2t1_nb(0:9) = (/ 1, 3, 7, 13, 22, 34, 50, 70, 95, 125 /)
!..procedures
CONTAINS
include 'sym2t1_gens.f90'
include 'sym2t1_prims.f90'
include 'sym2t1_secs.f90'
include 'sym2t1_base.f90'
END MODULE inv_sym2t1
