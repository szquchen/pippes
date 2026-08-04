MODULE inv_t1
!..use and access
use inv_wp
implicit none
private
public :: t1_prims, t1_secs, t1_base
!..data
! Note: all rather trivial.  We keep it this way to be consistent
! with other routines in the inv package.
integer, parameter, public :: &
  t1_nr=1, t1_ngrp=1, t1_ngen=0, &
  t1_dnpr(0:9) = (/ 0, 1, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  t1_npr(0:9) = (/ 0, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  t1_dnsc(0:9) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  t1_nsc(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  t1_dnb(0:9) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  t1_nb(0:9) = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 /)
!..procedures
interface t1_base
 module procedure t1_base_vec, t1_base_scal
end interface
CONTAINS
include 't1_prims.f90'
include 't1_secs.f90'
include 't1_base_vec.f90'
include 't1_base_scal.f90'
END MODULE inv_t1
