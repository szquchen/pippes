MODULE inv_mg33211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg33211_prims, mg33211_prib, mg33211_isecs, mg33211_secs, &
  mg33211_base
!..data
integer, parameter, private :: nkk=5, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,3,2,1,1/)
integer, parameter, public :: &
  mg33211_id=124, &
  mg33211_nkk=nkk, mg33211_nk=nk, mg33211_nr=nr, mg33211_ngrp=6*6*2, &
  mg33211_nkj(0:nkk-1)=nkj, &
  mg33211_dnpr(0:9) = (/ 0, 13, 17, 11, 1, 0, 3, 0, 0, 0 /), &
  mg33211_npr(0:9) = (/ 0, 13, 30, 41, 42, 42, 45, 45, 45, 45 /), &
  mg33211_dnpb(0:9) = (/ 1, 13, 108, 687, 3664, 17113, 72009, &
    278100, 999190, 3373602 /), &
  mg33211_npb(0:9) = (/ 1, 14, 122, 809, 4473, 21586, 93595, &
    371695, 1370885, 4744487 /), &
  mg33211_dnsc(0:9) = (/ 1, 0, 26, 146, 800, 4492, 21852, &
    93266, 360254, 1248209 /), &
  mg33211_nsc(0:9) = (/ 1, 1, 27, 173, 973, 5465, 27317, &
    120583, 480837, 1729046 /), &
  mg33211_dnb(0:9) = (/ 1, 13, 134, 1171, 9170, 65635, 434223, &
    2670060, 15319854, 82283167 /), &
  mg33211_nb(0:9) = (/ 1, 14, 148, 1319, 10489, 76124, 510347, &
    3180407, 18500261, 100783428 /)
!..procedures
CONTAINS
include 'mg33211_prims.f90'
include 'mg33211_prib.f90'
include 'mg33211_isecs.f90'
include 'mg33211_secs.f90'
include 'mg33211_base.f90'
END MODULE inv_mg33211
