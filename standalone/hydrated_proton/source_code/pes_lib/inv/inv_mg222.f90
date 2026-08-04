MODULE inv_mg222
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg222_prims, mg222_prib, mg222_isecs, mg222_secs, &
  mg222_base
!..data
integer, parameter, private :: nkk=3, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,2,2/)
integer, parameter, public :: &
  mg222_id=25, &
  mg222_nkk=nkk, mg222_nk=nk, mg222_nr=nr, mg222_ngrp=8, &
  mg222_nkj(0:nkk-1)=nkj, &
  mg222_dnpr(0:9) = (/ 0, 6, 9, 0, 0, 0, 0, 0, 0, 0 /), &
  mg222_npr(0:9) = (/ 0, 6, 15, 15, 15, 15, 15, 15, 15, 15 /), &
  mg222_dnpb(0:9) = (/ 1, 6, 30, 110, 360, 1026, 2706, 6570, 15075, &
    32680 /), &
  mg222_npb(0:9) = (/ 1, 7, 37, 147, 507, 1533, 4239, 10809, 25884, &
    58564 /), &
  mg222_dnsc(0:9) = (/ 1, 0, 3, 13, 15, 15, 13, 3, 0, 1 /), &
  mg222_nsc(0:9) = (/ 1, 1, 4, 17, 32, 47, 60, 63, 63, 64 /), &
  mg222_dnb(0:9) = (/ 1, 6, 33, 141, 543, 1851, 5769, 16509, 43989, &
    109879 /), &
  mg222_nb(0:9) = (/ 1, 7, 40, 181, 724, 2575, 8344, 24853, 68842, &
    178721 /)
!..procedures
CONTAINS
include 'mg222_prims.f90'
include 'mg222_prib.f90'
include 'mg222_isecs.f90'
include 'mg222_secs.f90'
include 'mg222_base.f90'
END MODULE inv_mg222
