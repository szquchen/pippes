MODULE inv_mg511
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg511_prims, mg511_prib, mg511_isecs, mg511_secs, &
  mg511_base
!..data
integer, parameter, private :: nkk=3, nk=7, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,1,1/)
integer, parameter, public :: &
  mg511_id=34, &
  mg511_nkk=nkk, mg511_nk=nk, mg511_nr=nr, mg511_ngrp=120, &
  mg511_nkj(0:nkk-1)=nkj, &
  mg511_dnpr(0:9) = (/ 0, 4, 4, 4, 4, 4, 1, 0, 0, 0 /), &
  mg511_npr(0:9) = (/ 0, 4, 8, 12, 16, 20, 21, 21, 21, 21 /), &
  mg511_dnpb(0:9) = (/ 1, 4, 14, 40, 105, 252, 571, 1224, 2518, 4984 /), &
  mg511_npb(0:9) = (/ 1, 5, 19, 59, 164, 416, 987, 2211, 4729, 9713 /), &
  mg511_dnsc(0:9) = (/ 1, 0, 3, 16, 54, 165, 482, 1303, 3245, 7379 /), &
  mg511_nsc(0:9) = (/ 1, 1, 4, 20, 74, 239, 721, 2024, 5269, 12648 /), &
  mg511_dnb(0:9) = (/ 1, 4, 17, 68, 265, 977, 3424, 11361, 35738, &
    106606 /), &
  mg511_nb(0:9) = (/ 1, 5, 22, 90, 355, 1332, 4756, 16117, 51855, &
    158461 /)
!..procedures
CONTAINS
include 'mg511_prims.f90'
include 'mg511_prib.f90'
include 'mg511_isecs.f90'
include 'mg511_secs.f90'
include 'mg511_base.f90'
END MODULE inv_mg511
