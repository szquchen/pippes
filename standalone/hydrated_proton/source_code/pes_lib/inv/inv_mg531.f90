MODULE inv_mg531
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg531_prims, mg531_prib, mg531_isecs, mg531_secs, &
  mg531_base
!..data
integer, parameter, private :: nkk=3, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,3,1/)
integer, parameter, public :: &
  mg531_id=74, &
  mg531_nkk=nkk, mg531_nk=nk, mg531_nr=nr, mg531_ngrp=120*6, &
  mg531_nkj(0:nkk-1)=nkj, &
  mg531_dnpr(0:9) = (/ 0, 5, 8, 8, 6, 4, 2, 0, 0, 0 /), &
  mg531_npr(0:9) = (/ 0, 5, 13, 21, 27, 31, 33, 33, 33, 33 /), &
  mg531_dnpb(0:9) = (/ 1, 5, 23, 83, 272, 804, 2226, 5786, 14336, &
    33980 /), &
  mg531_npb(0:9) = (/ 1, 6, 29, 112, 384, 1188, 3414, 9200, 23536, &
    57516 /), &
  mg531_dnsc(0:9) = (/ 1, 0, 6, 39, 165, 747, 3331, 13735, 53226, &
    192557 /), &
  mg531_nsc(0:9) = (/ 1, 1, 7, 46, 211, 958, 4289, 18024, 71250, &
    263807 /), &
  mg531_dnb(0:9) = (/ 1, 5, 29, 152, 770, 3771, 17956, 82484, 364443, &
    1542419 /), &
  mg531_nb(0:9) = (/ 1, 6, 35, 187, 957, 4728, 22684, 105168, 469611, &
    2012030 /)
!..procedures
CONTAINS
include 'mg531_prims.f90'
include 'mg531_prib.f90'
include 'mg531_isecs.f90'
include 'mg531_secs.f90'
include 'mg531_base.f90'
END MODULE inv_mg531
