MODULE inv_mg52111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg52111_prims, mg52111_prib, mg52111_isecs, mg52111_secs, &
  mg52111_base
!..data
integer, parameter, private :: nkk=5, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,2,1,1,1/)
integer, parameter, public :: &
  mg52111_id=121, &
  mg52111_nkk=nkk, mg52111_nk=nk, mg52111_nr=nr, mg52111_ngrp=120*2, &
  mg52111_nkj(0:nkk-1)=nkj, &
  mg52111_dnpr(0:9) = (/ 0, 12, 11, 6, 7, 6, 2, 0, 0, 0 /), &
  mg52111_npr(0:9) = (/ 0, 12, 23, 29, 36, 42, 44, 44, 44, 44 /), &
  mg52111_dnpb(0:9) = (/ 1, 12, 89, 502, 2368, 9788, 36519, 125386, &
    401715, 1213284 /), &
  mg52111_npb(0:9) = (/ 1, 13, 102, 604, 2972, 12760, 49279, 174665, &
    576380, 1789664 /), &
  mg52111_dnsc(0:9) = (/ 1, 0, 16, 76, 426, 2278, 11234, 51015, &
    213894, 824913 /), &
  mg52111_nsc(0:9) = (/ 1, 1, 17, 93, 519, 2797, 14031, 65046, &
    278940, 1103853 /), &
  mg52111_dnb(0:9) = (/ 1, 12, 105, 770, 5130, 31974, 189043, 1064379, &
    5708131, 29130340 /), &
  mg52111_nb(0:9) = (/ 1, 13, 118, 888, 6018, 37992, 227035, 1291414, &
    6999545, 36129885 /)
!..procedures
CONTAINS
include 'mg52111_prims.f90'
include 'mg52111_prib.f90'
include 'mg52111_isecs.f90'
include 'mg52111_secs.f90'
include 'mg52111_base.f90'
END MODULE inv_mg52111
