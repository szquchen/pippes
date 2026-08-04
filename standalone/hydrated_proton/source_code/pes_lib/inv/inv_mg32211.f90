MODULE inv_mg32211
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg32211_prims, mg32211_prib, mg32211_isecs, mg32211_secs, &
  mg32211_base
!..data
integer, parameter, private :: nkk=5, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,2,1,1/)
integer, parameter, public :: &
  mg32211_id=88, &
  mg32211_nkk=nkk, mg32211_nk=nk, mg32211_nr=nr, mg32211_ngrp=24, &
  mg32211_nkj(0:nkk-1)=nkj, &
  mg32211_dnpr(0:9) = (/ 0, 13, 16, 5, 0, 0, 2, 0, 0, 0 /), &
  mg32211_npr(0:9) = (/ 0, 13, 29, 34, 34, 34, 36, 36, 36, 36 /), &
  mg32211_dnpb(0:9) = (/ 1, 13, 107, 668, 3477, 15771, 64208, 239165, &
    826645, 2679070 /), &
  mg32211_npb(0:9) = (/ 1, 14, 121, 789, 4266, 20037, 84245, 323410, &
    1150055, 3829125 /), &
  mg32211_dnsc(0:9) = (/ 1, 0, 22, 97, 441, 1895, 6703, 19679, &
    51279, 116932 /), &
  mg32211_nsc(0:9) = (/ 1, 1, 23, 120, 561, 2456, 9159, 28838, &
    80117, 197049 /), &
  mg32211_dnb(0:9) = (/ 1, 13, 129, 1051, 7533, 48474, 284023, 1527567, &
    7592552, 35079618 /), &
  mg32211_nb(0:9) = (/ 1, 14, 143, 1194, 8727, 57201, 341224, 1868791, &
    9461343, 44540961 /)
!..procedures
CONTAINS
include 'mg32211_prims.f90'
include 'mg32211_prib.f90'
include 'mg32211_isecs.f90'
include 'mg32211_secs.f90'
include 'mg32211_base.f90'
END MODULE inv_mg32211
