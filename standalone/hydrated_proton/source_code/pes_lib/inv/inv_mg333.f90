MODULE inv_mg333
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg333_prims, mg333_prib, mg333_isecs, mg333_secs, &
  mg333_base
!..data
integer, parameter, private :: nkk=3, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,3/)
integer, parameter, public :: &
  mg333_id=78, &
  mg333_nkk=nkk, mg333_nk=nk, mg333_nr=nr, mg333_ngrp=6*6*6, &
  mg333_nkj(0:nkk-1)=nkj, &
  mg333_dnpr(0:9) = (/ 0, 6, 12, 12, 3, 0, 3, 0, 0, 0 /), &
  mg333_npr(0:9) = (/ 0, 6, 18, 30, 33, 33, 36, 36, 36, 36 /), &
  mg333_dnpb(0:9) = (/ 1, 6, 33, 140, 531, 1806, 5692, 16746, 46635, &
    123616 /), &
  mg333_npb(0:9) = (/ 1, 7, 40, 180, 711, 2517, 8209, 24955, 71590, &
    195206 /), &
  mg333_dnsc(0:9) = (/ 1, 0, 9, 67, 294, 1368, 6242, 25188, 92718, &
    312300 /), &
  mg333_nsc(0:9) = (/ 1, 1, 10, 77, 371, 1739, 7981, 33169, 125887, &
    438187 /), &
  mg333_dnb(0:9) = (/ 1, 6, 42, 261, 1524, 8409, 44003, 217521, 1016331, &
    4486758 /), &
  mg333_nb(0:9) = (/ 1, 7, 49, 310, 1834, 10243, 54246, 271767, 1288098, &
    5774856 /)
!..procedures
CONTAINS
include 'mg333_prims.f90'
include 'mg333_prib.f90'
include 'mg333_isecs.f90'
include 'mg333_secs.f90'
include 'mg333_base.f90'
END MODULE inv_mg333
