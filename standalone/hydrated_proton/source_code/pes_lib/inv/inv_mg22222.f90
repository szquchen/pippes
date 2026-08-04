MODULE inv_mg22222
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg22222_prims, mg22222_prib, mg22222_isecs, mg22222_secs, &
  mg22222_base
!..data
integer, parameter, private :: nkk=5, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,2,2,2/)
integer, parameter, public :: &
  mg22222_id=126, &
  mg22222_nkk=nkk, mg22222_nk=nk, mg22222_nr=nr, mg22222_ngrp=32, &
  mg22222_nkj(0:nkk-1)=nkj, &
  mg22222_dnpr(0:9) = (/ 0, 15, 30, 0, 0, 0, 0, 0, 0, 0 /), &
  mg22222_npr(0:9) = (/ 0, 15, 45, 45, 45, 45, 45, 45, 45, 45 /), &
  mg22222_dnpb(0:9) = (/ 1, 15, 150, 1130, 7125, 39003, 191320, &
    855720, 3541590, 13699210 /), &
  mg22222_npb(0:9) = (/ 1, 16, 166, 1296, 8421, 47424, 238744, &
    1094464, 4636054, 18335264 /), &
  mg22222_dnsc(0:9) = (/ 1, 0, 30, 170, 860, 4472, 18265, &
    62580, 183255, 450200 /), &
  mg22222_nsc(0:9) = (/ 1, 1, 31, 201, 1061, 5533, 23798, &
    86378, 269633, 719833 /), &
  mg22222_dnb(0:9) = (/ 1, 15, 180, 1750, 15035, 115775, 811515, &
    5216215, 30954265, 170526265 /), &
  mg22222_nb(0:9) = (/ 1, 16, 196, 1946, 16981, 132756, 944271, &
    6160486, 37114751, 207641016 /)
!..procedures
CONTAINS
include 'mg22222_prims.f90'
include 'mg22222_prib.f90'
include 'mg22222_isecs.f90'
include 'mg22222_secs.f90'
include 'mg22222_base.f90'
END MODULE inv_mg22222
