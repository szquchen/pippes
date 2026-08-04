MODULE inv_mg3111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3111_prims, mg3111_prib, mg3111_isecs, mg3111_secs, &
  mg3111_base
!..data
integer, parameter, private :: nkk=4, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,1,1,1/)
integer, parameter, public :: &
  mg3111_id=26, &
  mg3111_nkk=nkk, mg3111_nk=nk, mg3111_nr=nr, mg3111_ngrp=6, &
  mg3111_nkj(0:nkk-1)=nkj, &
  mg3111_dnpr(0:9) = (/ 0, 7, 4, 4, 0, 0, 0, 0, 0, 0 /), &
  mg3111_npr(0:9) = (/ 0, 7, 11, 15, 15, 15, 15, 15, 15, 15 /), &
  mg3111_dnpb(0:9) = (/ 1, 7, 32, 116, 360, 996, 2522, 5942, 13186, &
    27810 /), &
  mg3111_npb(0:9) = (/ 1, 8, 40, 156, 516, 1512, 4034, 9976, 23162, &
    50972 /), &
  mg3111_dnsc(0:9) = (/ 1, 0, 6, 16, 21, 36, 56, 36, 21, 16 /), &
  mg3111_nsc(0:9) = (/ 1, 1, 7, 23, 44, 80, 136, 172, 193, 209 /), &
  mg3111_dnb(0:9) = (/ 1, 7, 38, 174, 685, 2387, 7518, 21694, 58055, &
    145501 /), &
  mg3111_nb(0:9) = (/ 1, 8, 46, 220, 905, 3292, 10810, 32504, 90559, &
    236060 /)
!..procedures
CONTAINS
include 'mg3111_prims.f90'
include 'mg3111_prib.f90'
include 'mg3111_isecs.f90'
include 'mg3111_secs.f90'
include 'mg3111_base.f90'
END MODULE inv_mg3111
