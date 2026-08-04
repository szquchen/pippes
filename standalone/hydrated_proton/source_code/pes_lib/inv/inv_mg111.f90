MODULE inv_mg111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg111_prims, mg111_prib, mg111_secs, mg111_base
!..data
integer, parameter, private :: nkk=3, nk=3, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/1,1,1/)
integer, parameter, public :: &
  mg111_id=6, &
  mg111_nkk=nkk, mg111_nk=nk, mg111_nr=nr, mg111_ngrp=1, &
  mg111_nkj(0:nkk-1)=nkj, &
  mg111_dnpr(0:19) = (/ 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg111_npr(0:19) = (/ 0, 3, 3, 3, 3, 3, 3, 3, 3, 3, &
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3 /), &
  mg111_dnpb(0:19) = (/ 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, &
    66, 78, 91, 105, 120, 136, 153, 171, 190, 210 /), &
  mg111_npb(0:19) = (/ 1, 4, 10, 20, 35, 56, 84, 120, 165, 220, &
    286, 364, 455, 560, 680, 816, 969, 1140, 1330, 1540 /), &
  mg111_dnsc(0:19) = (/ 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, &
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /), &
  mg111_nsc(0:19) = (/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, &
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1 /), &
  mg111_dnb(0:19) = (/ 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, &
    66, 78, 91, 105, 120, 136, 153, 171, 190, 210 /), &
  mg111_nb(0:19) = (/ 1, 4, 10, 20, 35, 56, 84, 120, 165, 220, &
    286, 364, 455, 560, 680, 816, 969, 1140, 1330, 1540 /)
!..procedures
CONTAINS
include 'mg111_prims.f90'
include 'mg111_prib.f90'
include 'mg111_secs.f90'
include 'mg111_base.f90'
END MODULE inv_mg111
