MODULE inv_cg7
!..use and access
use inv_wp
implicit none
private
public :: cg7_gens, cg7_prims, cg7_prib, cg7_secs, cg7_base
!..data
integer, parameter, private :: nk=7, nr=nk*(nk-1)/2
integer, parameter, public :: &
  cg7_nk=nk, cg7_nr=nr, cg7_ngrp=5040, cg7_ngen=2, &
  cg7_dnpr(0:9) = (/ 0, 1, 2, 5, 3, 3, 2, 3, 0, 0 /), &
  cg7_npr(0:9) = (/ 0, 1, 3, 8, 11, 14, 16, 19, 19, 19 /), &
  cg7_dnpb(0:9) = (/ 1, 1, 3, 8, 14, 27, 54, 93, 162, 281 /), &
  cg7_npb(0:9) = (/ 1, 2, 5, 13, 27, 54, 108, 201, 363, 644 /), &
  cg7_dnsc(0:9) = (/ 1, 0, 0, 0, 8, 25, 70, 169, 450, 1146 /), &
  cg7_nsc(0:9) = (/ 1, 1, 1, 1, 9, 34, 104, 273, 723, 1869 /), &
  cg7_dnb(0:9) = (/ 1, 1, 3, 8, 22, 60, 173, 471, 1303, 3510 /), &
  cg7_nb(0:9) = (/ 1, 2, 5, 13, 35, 95, 268, 739, 2042, 5552 /)
!..procedures
CONTAINS
include 'cg7_gens.f90'
include 'cg7_prims.f90'
include 'cg7_prib.f90'
include 'cg7_secs.f90'
include 'cg7_base.f90'
END MODULE inv_cg7
