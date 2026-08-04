MODULE inv_cg8
!..use and access
use inv_wp
implicit none
private
!! public :: cg8_gens, cg8_prims, cg8_prib, cg8_secs, cg8_base
!..data
integer, parameter, private :: nk=8, nr=nk*(nk-1)/2
integer, parameter, public :: &
  cg8_nk=nk, cg8_nr=nr, cg8_ngrp=40320, cg8_ngen=2, &
  cg8_dnpr(0:9) = (/ 
  cg8_npr(0:9) = (/ 
  cg8_dnpb(0:9) = (/ 
  cg8_npb(0:9) = (/ 
  cg8_dnsc(0:9) = (/ 
  cg8_nsc(0:9) = (/ 
  cg8_dnb(0:9) = (/ 
  cg8_nb(0:9) = (/ 
! Note: there is also a primary at degree ..
!..procedures
CONTAINS
include 'cg8_gens.f90'
!! include 'cg8_prims.f90'
include 'cg8_prib.f90'
!! include 'cg8_secs.f90'
include 'cg8_base.f90'
END MODULE inv_cg8
