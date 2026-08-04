MODULE inv_mg222211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg222211_prims, mg222211_prib, mg222211_isecs, mg222211_secs, &
  mg222211_base
!..data
integer, parameter, private :: nkk=6, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,2,2,1,1/)
integer, parameter, public :: &
  mg222211_id=131, &
  mg222211_nkk=nkk, mg222211_nk=nk, mg222211_nr=nr, mg222211_ngrp=16, &
  mg222211_nkj(0:nkk-1)=nkj, &
  mg222211_dnpr(0:9) = (/ 0, 19, 26, 0, 0, 0, 0, 0, 0, 0 /), &
  mg222211_npr(0:9) = (/ 0, 19, 45, 45, 45, 45, 45, 45, 45, 45 /), &
  mg222211_dnpb(0:9) = (/ 1, 19, 216, 1824, 12606, 74898, 394752, &
    1884648, 8275527, 33804173 /), &
  mg222211_npb(0:9) = (/ 1, 20, 236, 2060, 14666, 89564, 484316, &
    2368964, 10644491, 44448664 /), &
  mg222211_dnsc(0:9) = (/ 1, 0, 40, 154, 923, 4060, 14095, &
    41490, 98530, 194640 /), &
  mg222211_nsc(0:9) = (/ 1, 1, 41, 195, 1118, 5178, 19273, &
    60763, 159293, 353933 /), &
  mg222211_dnb(0:9) = (/ 1, 19, 256, 2738, 25095, 202719, 1470491, &
    9691699, 58572037, 327030945 /), &
  mg222211_nb(0:9) = (/ 1, 20, 276, 3014, 28109, 230828, 1701319, &
    11393018, 69965055, 396996000 /)
!..procedures
CONTAINS
include 'mg222211_prims.f90'
include 'mg222211_prib.f90'
include 'mg222211_isecs.f90'
include 'mg222211_secs.f90'
include 'mg222211_base.f90'
END MODULE inv_mg222211
