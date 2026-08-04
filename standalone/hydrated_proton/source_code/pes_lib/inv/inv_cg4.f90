MODULE inv_cg4
!..use and access
use inv_wp
implicit none
private
public :: cg4_gens, cg4_prims, cg4_prib, cg4_secs, cg4_base
!..data
integer, parameter, private :: nk=4, nr=nk*(nk-1)/2
integer, parameter, public :: &
  cg4_nk=nk, cg4_nr=nr, cg4_ngrp=24, cg4_ngen=2, &
  cg4_dnpr(0:9) = (/ 0, 1, 2, 2, 1, 0, 0, 0, 0, 0 /), &
  cg4_npr(0:9) = (/ 0, 1, 3, 5, 6, 6, 6, 6, 6, 6 /), &
  cg4_dnpb(0:9) = (/ 1, 1, 3, 5, 9, 13, 22, 30, 45, 61 /), &
  cg4_npb(0:9) = (/ 1, 2, 5, 10, 19, 32, 54, 84, 129, 190 /), &
  cg4_dnsc(0:9) = (/ 1, 0, 0, 1, 1, 1, 1, 0, 0, 1 /), &
  cg4_nsc(0:9) = (/ 1, 1, 1, 2, 3, 4, 5, 5, 5, 6 /), &
  cg4_dnb(0:9) = (/ 1, 1, 3, 6, 11, 18, 32, 48, 75, 111 /), &
  cg4_nb(0:9) = (/ 1, 2, 5, 11, 22, 40, 72, 120, 195, 306 /)
!..procedures
CONTAINS
include 'cg4_gens.f90'
include 'cg4_prims.f90'
include 'cg4_prib.f90'
include 'cg4_secs.f90'
include 'cg4_base.f90'
END MODULE inv_cg4
