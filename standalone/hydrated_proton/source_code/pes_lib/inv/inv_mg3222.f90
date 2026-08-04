MODULE inv_mg3222
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg3222_prims, mg3222_prib, mg3222_isecs, mg3222_secs, &
  mg3222_base
!..data
integer, parameter, private :: nkk=4, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,2,2/)
integer, parameter, public :: &
  mg3222_id=84, &
  mg3222_nkk=nkk, mg3222_nk=nk, mg3222_nr=nr, mg3222_ngrp=6*2*2*2, &
  mg3222_nkj(0:nkk-1)=nkj, &
  mg3222_dnpr(0:9) = (/ 0, 10, 19, 4, 0, 0, 3, 0, 0, 0 /), &
  mg3222_npr(0:9) = (/ 0, 10, 29, 33, 33, 33, 36, 36, 36, 36 /), &
  mg3222_dnpb(0:9) = (/ 1, 10, 74, 414, 1990, 8378, 32023, 112508, &
    369010, 1138952 /), &
  mg3222_npb(0:9) = (/ 1, 11, 85, 499, 2489, 10867, 42890, 155398, &
    524408, 1663360 /), &
  mg3222_dnsc(0:9) = (/ 1, 0, 15, 95, 393, 1761, 7087, 23370, &
    67542, 175168 /), &
  mg3222_nsc(0:9) = (/ 1, 1, 16, 111, 504, 2265, 9352, 32722, &
    100264, 275432 /), &
  mg3222_dnb(0:9) = (/ 1, 10, 89, 659, 4443, 27309, 154982, 814484, &
    3982069, 18179687 /), &
  mg3222_nb(0:9) = (/ 1, 11, 100, 759, 5202, 32511, 187493, 1001977, &
    4984046, 23163733 /)
!..procedures
CONTAINS
include 'mg3222_prims.f90'
include 'mg3222_prib.f90'
include 'mg3222_isecs.f90'
include 'mg3222_secs.f90'
include 'mg3222_base.f90'
END MODULE inv_mg3222
