MODULE inv_mg222111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg222111_prims, mg222111_prib, mg222111_isecs, mg222111_secs, &
  mg222111_base
!..data
integer, parameter, private :: nkk=6, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,2,1,1,1/)
integer, parameter, public :: &
  mg222111_id=92, &
  mg222111_nkk=nkk, mg222111_nk=nk, mg222111_nr=nr, mg222111_ngrp=8, &
  mg222111_nkj(0:nkk-1)=nkj, &
  mg222111_dnpr(0:9) = (/ 0, 18, 18, 0, 0, 0, 0, 0, 0, 0 /), &
  mg222111_npr(0:9) = (/ 0, 18, 36, 36, 36, 36, 36, 36, 36, 36 /), &
  mg222111_dnpb(0:9) = (/ 1, 18, 189, 1464, 9234, 49932, 239058, &
    1035576, 4122981, 15264866 /), &
  mg222111_npb(0:9) = (/ 1, 19, 208, 1672, 10906, 60838, 299896, &
    1335472, 5458453, 20723319 /), &
  mg222111_dnsc(0:9) = (/ 1, 0, 30, 76, 390, 1080, 2350, &
    4020, 5280, 6160 /), &
  mg222111_nsc(0:9) = (/ 1, 1, 31, 107, 497, 1577, 3927, &
    7947, 13227, 19387 /), &
  mg222111_dnb(0:9) = (/ 1, 18, 219, 2080, 16662, 116316, 722842, &
    4056720, 20793723, 98248134 /), &
  mg222111_nb(0:9) = (/ 1, 19, 238, 2318, 18980, 135296, 858138, &
    4914858, 25708581, 123956715 /)
!..procedures
CONTAINS
include 'mg222111_prims.f90'
include 'mg222111_prib.f90'
include 'mg222111_isecs.f90'
include 'mg222111_secs.f90'
include 'mg222111_base.f90'
END MODULE inv_mg222111
