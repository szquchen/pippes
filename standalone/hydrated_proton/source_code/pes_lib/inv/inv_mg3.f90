MODULE inv_mg3
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3_prims, mg3_prib, mg3_secs, mg3_base
!..data
integer, parameter, private :: nkk=1, nk=3, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3/)
integer, parameter, public :: &
  mg3_id=4, &
  mg3_nkk=nkk, mg3_nk=nk, mg3_nr=nr, mg3_ngrp=6, &
  mg3_nkj(0:nkk-1)=nkj, &
  mg3_dnpr(0:19) = (/ 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg3_npr(0:19) = (/ 0, 1, 2, 3, 3, 3, 3, 3, 3, 3, &
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3 /), &
  mg3_dnpb(0:19) = (/ 1, 1, 2, 3, 4, 5, 7, 8, 10, 12, &
    14, 16, 19, 21, 24, 27, 30, 33, 37, 40 /), &
  mg3_npb(0:19) = (/ 1, 2, 4, 7, 11, 16, 23, 31, 41, 53, &
    67, 83, 102, 123, 147, 174, 204, 237, 274, 314 /), &
  mg3_dnsc(0:19) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg3_nsc(0:19) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  mg3_dnb(0:19) = (/ 1, 1, 2, 3, 4, 5, 7, 8, 10, 12, &
    14, 16, 19, 21, 24, 27, 30, 33, 37, 40 /), &
  mg3_nb(0:19) = (/ 1, 2, 4, 7, 11, 16, 23, 31, 41, 53, &
    67, 83, 102, 123, 147, 174, 204, 237, 274, 314 /)
!..procedures
CONTAINS
include 'mg3_prims.f90'
include 'mg3_prib.f90'
include 'mg3_secs.f90'
include 'mg3_base.f90'
END MODULE inv_mg3
