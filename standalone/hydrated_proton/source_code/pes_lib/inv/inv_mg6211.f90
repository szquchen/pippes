MODULE inv_mg6211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg6211_prims, mg6211_prib, mg6211_isecs, mg6211_secs, &
  mg6211_base
!..data
integer, parameter, private :: nkk=4, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,2,1,1/)
integer, parameter, public :: &
  mg6211_id=112, &
  mg6211_nkk=nkk, mg6211_nk=nk, mg6211_nr=nr, mg6211_ngrp=720*2, &
  mg6211_nkj(0:nkk-1)=nkj, &
  mg6211_dnpr(0:9) = (/ 0, 8, 9, 8, 7, 6, 6, 0, 0, 0 /), &
  mg6211_npr(0:9) = (/ 0, 8, 17, 25, 32, 38, 44, 44, 44, 44 /), &
  mg6211_dnpb(0:9) = (/ 1, 8, 45, 200, 770, 2654, 8412, 24878, &
    69479, 184716 /), &
  mg6211_npb(0:9) = (/ 1, 9, 54, 254, 1024, 3678, 12090, 36968, &
    106447, 291163 /), &
  mg6211_dnsc(0:9) = (/ 1, 0, 9, 45, 228, 1135, 5610, 26099, &
    115777, 482681 /), &
  mg6211_nsc(0:9) = (/ 1, 1, 10, 55, 283, 1418, 7028, 33127, &
    148904, 631585 /), &
  mg6211_dnb(0:9) = (/ 1, 8, 54, 317, 1763, 9438, 49292, 251068, &
    1244196, 5971572 /), &
  mg6211_nb(0:9) = (/ 1, 9, 63, 380, 2143, 11581, 60873, 311941, &
    1556137, 7527709 /)
!..procedures
CONTAINS
include 'mg6211_prims.f90'
include 'mg6211_prib.f90'
include 'mg6211_isecs.f90'
include 'mg6211_secs.f90'
include 'mg6211_base.f90'
END MODULE inv_mg6211
