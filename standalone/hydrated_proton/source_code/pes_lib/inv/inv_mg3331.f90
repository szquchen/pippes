MODULE inv_mg3331
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3331_prims, mg3331_prib, mg3331_isecs, mg3331_secs, &
  mg3331_base
!..data
integer, parameter, private :: nkk=4, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,3,1/)
integer, parameter, public :: &
  mg3331_id=117, &
  mg3331_nkk=nkk, mg3331_nk=nk, mg3331_nr=nr, mg3331_ngrp=6*6*6, &
  mg3331_nkj(0:nkk-1)=nkj, &
  mg3331_dnpr(0:9) = (/ 0, 9, 15, 15, 3, 0, 3, 0, 0, 0 /), &
  mg3331_npr(0:9) = (/ 0, 9, 24, 39, 42, 42, 45, 45, 45, 45 /), &
  mg3331_dnpb(0:9) = (/ 1, 9, 60, 315, 1428, 5769, 21311, 73062, &
    235266, 717540 /), &
  mg3331_npb(0:9) = (/ 1, 10, 70, 385, 1813, 7582, 28893, 101955, &
    337221, 1054761 /), &
  mg3331_dnsc(0:9) = (/ 1, 0, 18, 121, 633, 3624, 19011, 89439, &
    387351, 1534498 /), &
  mg3331_nsc(0:9) = (/ 1, 1, 19, 140, 773, 4397, 23408, 112847, &
    500198, 2034696 /), &
  mg3331_dnb(0:9) = (/ 1, 9, 78, 598, 4230, 28020, 174737, 1027065, &
    5695359, 29813598 /), &
  mg3331_nb(0:9) = (/ 1, 10, 88, 686, 4916, 32936, 207673, 1234738, &
    6930097, 36743695 /)
!..procedures
CONTAINS
include 'mg3331_prims.f90'
include 'mg3331_prib.f90'
include 'mg3331_isecs.f90'
include 'mg3331_secs.f90'
include 'mg3331_base.f90'
END MODULE inv_mg3331
