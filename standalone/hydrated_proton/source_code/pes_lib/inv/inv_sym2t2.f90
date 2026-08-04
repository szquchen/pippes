MODULE inv_sym2t2
!..use and access
use inv_wp
implicit none
private
public :: sym2t2_gens, sym2t2_prims, sym2t2_secs, sym2t2_base
!..data
integer, parameter, public :: &
  sym2t2_nr=4, sym2t2_ngrp=2, sym2t2_ngen=1, &
  sym2t2_dnpr(0:9) = (/ 0, 3, 1, 0, 0, 0, 0, 0, 0, 0 /), &
  sym2t2_npr(0:9) = (/ 0, 3, 4, 4, 4, 4, 4, 4, 4, 4 /), &
  sym2t2_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  sym2t2_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  sym2t2_dnb(0:9) = (/ 1, 3, 7, 13, 22, 34, 50, 70, 95, 125 /), &
  sym2t2_nb(0:9) = (/ 1, 4, 11, 24, 46, 80, 130, 200, 295, 420 /)
!..procedures
CONTAINS
include 'sym2t2_gens.f90'
include 'sym2t2_prims.f90'
include 'sym2t2_secs.f90'
include 'sym2t2_base.f90'
END MODULE inv_sym2t2
