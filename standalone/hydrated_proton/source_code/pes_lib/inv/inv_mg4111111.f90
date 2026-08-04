MODULE inv_mg4111111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg4111111_prims, mg4111111_prib, mg4111111_isecs, mg4111111_secs, &
  mg4111111_base
!..data
integer, parameter, private :: nkk=7, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,1,1,1,1,1,1/)
integer, parameter, public :: &
  mg4111111_id=132, &
  mg4111111_nkk=nkk, mg4111111_nk=nk, mg4111111_nr=nr, mg4111111_ngrp=24, &
  mg4111111_nkj(0:nkk-1)=nkj, &
  mg4111111_dnpr(0:9) = (/ 0, 22, 8, 8, 7, 0, 0, 0, 0, 0 /), &
  mg4111111_npr(0:9) = (/ 0, 22, 30, 38, 45, 45, 45, 45, 45, 45 /), &
  mg4111111_dnpb(0:9) = (/ 1, 22, 261, 2208, 14893, 85006, 425901, 1919712, &
    7922053, 30323094 /), &
  mg4111111_npb(0:9) = (/ 1, 23, 284, 2492, 17385, 102391, 528292, 2448004, &
    10370057, 40693151 /), &
  mg4111111_dnsc(0:9) = (/ 1, 0, 21, 105, 574, 2527, 10360, 35756, &
    110621, 304269 /), &
  mg4111111_nsc(0:9) = (/ 1, 1, 22, 127, 701, 3228, 13588, 49344, &
    159965, 464234 /), &
  mg4111111_dnb(0:9) = (/ 1, 22, 282, 2775, 23258, 173934, 1186262, 7459218, &
    43521015, 236729833 /), &
  mg4111111_nb(0:9) = (/ 1, 23, 305, 3080, 26338, 200272, 1386534, 8845752, &
    52366767, 289096600 /)
!..procedures
CONTAINS
include 'mg4111111_prims.f90'
include 'mg4111111_prib.f90'
include 'mg4111111_isecs.f90'
include 'mg4111111_secs.f90'
include 'mg4111111_base.f90'
END MODULE inv_mg4111111
