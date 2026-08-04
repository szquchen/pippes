MODULE inv_mg532
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg532_prims, mg532_prib, mg532_isecs, mg532_secs, &
  mg532_base
!..data
integer, parameter, private :: nkk=3, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,3,2/)
integer, parameter, public :: &
  mg532_id=108, &
  mg532_nkk=nkk, mg532_nk=nk, mg532_nr=nr, mg532_ngrp=120*6*2, &
  mg532_nkj(0:nkk-1)=nkj, &
  mg532_dnpr(0:9) = (/ 0, 6, 12, 8, 7, 4, 4, 0, 0, 0 /), &
  mg532_npr(0:9) = (/ 0, 6, 18, 26, 33, 37, 41, 41, 41, 41 /), &
  mg532_dnpb(0:9) = (/ 1, 6, 33, 136, 511, 1702, 5295, 15340, &
    42224, 110620 /), &
  mg532_npb(0:9) = (/ 1, 7, 40, 176, 687, 2389, 7684, 23024, &
    65248, 175868 /), &
  mg532_dnsc(0:9) = (/ 1, 0, 7, 59, 275, 1398, 7278, 35140, &
    160160, 686591 /), &
  mg532_nsc(0:9) = (/ 1, 1, 8, 67, 342, 1740, 9018, 44158, &
    204318, 890909 /), &
  mg532_dnb(0:9) = (/ 1, 6, 40, 237, 1371, 7649, 41637, 219745, &
    1121534, 5509812 /), &
  mg532_nb(0:9) = (/ 1, 7, 47, 284, 1655, 9304, 50941, 270686, &
    1392220, 6902032 /)
!..procedures
CONTAINS
include 'mg532_prims.f90'
include 'mg532_prib.f90'
include 'mg532_isecs.f90'
include 'mg532_secs.f90'
include 'mg532_base.f90'
END MODULE inv_mg532
