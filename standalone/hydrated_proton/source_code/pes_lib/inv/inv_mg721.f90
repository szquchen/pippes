MODULE inv_mg721
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg721_prims, mg721_prib, mg721_isecs, mg721_secs, &
  mg721_base
!..data
integer, parameter, private :: nkk=3, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/7,2,1/)
integer, parameter, public :: &
  mg721_id=104, &
  mg721_nkk=nkk, mg721_nk=nk, mg721_nr=nr, mg721_ngrp=5040*2, &
  mg721_nkj(0:nkk-1)=nkj, &
  mg721_dnpr(0:9) = (/ 0, 5, 7, 8, 6, 5, 5, 5, 0, 0 /), &
  mg721_npr(0:9) = (/ 0, 5, 12, 20, 26, 31, 36, 41, 41, 41 /), &
  mg721_dnpb(0:9) = (/ 1, 5, 22, 78, 249, 722, 1962, 5024, &
    12279, 28787 /), &
  mg721_npb(0:9) = (/ 1, 6, 28, 106, 355, 1077, 3039, 8063, &
    20342, 49129 /), &
  mg721_dnsc(0:9) = (/ 1, 0, 4, 23, 110, 480, 2217, 9993, &
    44090, 187052 /), &
  mg721_nsc(0:9) = (/ 1, 1, 5, 28, 138, 618, 2835, 12828, &
    56918, 243970 /), &
  mg721_dnb(0:9) = (/ 1, 5, 26, 121, 562, 2570, 11789, 53857, &
    244392, 1093223 /), &
  mg721_nb(0:9) = (/ 1, 6, 32, 153, 715, 3285, 15074, 68931, &
    313323, 1406546 /)
!..procedures
CONTAINS
include 'mg721_prims.f90'
include 'mg721_prib.f90'
include 'mg721_isecs.f90'
include 'mg721_secs.f90'
include 'mg721_base.f90'
END MODULE inv_mg721
