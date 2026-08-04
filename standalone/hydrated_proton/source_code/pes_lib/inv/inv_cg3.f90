MODULE inv_cg3
!..use and access
use inv_wp
implicit none
private
public :: cg3_gens, cg3_prims, cg3_prib, cg3_secs, cg3_base
!..data
integer, parameter, private :: nk=3, nr=nk*(nk-1)/2
integer, parameter, public :: &
  cg3_nk=nk, cg3_nr=nr, cg3_ngrp=6, cg3_ngen=2, &
  cg3_dnpr(0:9) = (/ 0, 1, 1, 1, 0, 0, 0, 0, 0, 0 /), &
  cg3_npr(0:9) = (/ 0, 1, 2, 3, 3, 3, 3, 3, 3, 3 /), &
  cg3_dnpb(0:9) = (/ 1, 1, 2, 3, 4, 5, 7, 8, 10, 12 /), &
  cg3_npb(0:9) = (/ 1, 2, 4, 7, 11, 16, 23, 31, 41, 53 /), &
  cg3_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  cg3_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  cg3_dnb(0:9) = (/ 1, 1, 2, 3, 4, 5, 7, 8, 10, 12 /), &
  cg3_nb(0:9) = (/ 1, 2, 4, 7, 11, 16, 23, 31, 41, 53 /)
!..procedures
CONTAINS
include 'cg3_gens.f90'
include 'cg3_prims.f90'
include 'cg3_prib.f90'
include 'cg3_secs.f90'
include 'cg3_base.f90'
END MODULE inv_cg3
