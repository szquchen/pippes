MODULE inv_mg2211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg2211_prims, mg2211_prib, mg2211_isecs, mg2211_secs, &
  mg2211_base
!..data
integer, parameter, private :: nkk=4, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,1,1/)
integer, parameter, public :: &
  mg2211_id=27, &
  mg2211_nkk=nkk, mg2211_nk=nk, mg2211_nr=nr, mg2211_ngrp=4, &
  mg2211_nkj(0:nkk-1)=nkj, &
  mg2211_dnpr(0:9) = (/ 0, 8, 7, 0, 0, 0, 0, 0, 0, 0 /), &
  mg2211_npr(0:9) = (/ 0, 8, 15, 15, 15, 15, 15, 15, 15, 15 /), &
  mg2211_dnpb(0:9) = (/ 1, 8, 43, 176, 610, 1856, 5118, 13008, 30921, &
    69400 /), &
  mg2211_npb(0:9) = (/ 1, 9, 52, 228, 838, 2694, 7812, 20820, 51741, &
    121141 /), &
  mg2211_dnsc(0:9) = (/ 1, 0, 6, 9, 9, 6, 0, 1, 0, 0 /), &
  mg2211_nsc(0:9) = (/ 1, 1, 7, 16, 25, 31, 31, 32, 32, 32 /), &
  mg2211_dnb(0:9) = (/ 1, 8, 49, 233, 949, 3377, 10797, 31477, 84887, &
    213917 /), &
  mg2211_nb(0:9) = (/ 1, 9, 58, 291, 1240, 4617, 15414, 46891, 131778, &
    345695 /)
!..procedures
CONTAINS
include 'mg2211_prims.f90'
include 'mg2211_prib.f90'
include 'mg2211_isecs.f90'
include 'mg2211_secs.f90'
include 'mg2211_base.f90'
END MODULE inv_mg2211
