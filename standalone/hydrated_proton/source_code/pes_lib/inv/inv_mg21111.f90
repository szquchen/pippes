MODULE inv_mg21111
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg21111_prims, mg21111_prib, mg21111_isecs, mg21111_secs, &
  mg21111_base
!..data
integer, parameter, private :: nkk=5, nk=6, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2,1,1,1,1/)
integer, parameter, public :: &
  mg21111_id=28, &
  mg21111_nkk=nkk, mg21111_nk=nk, mg21111_nr=nr, mg21111_ngrp=2, &
  mg21111_nkj(0:nkk-1)=nkj, &
  mg21111_dnpr(0:9) = (/ 0, 11, 4, 0, 0, 0, 0, 0, 0, 0 /), &
  mg21111_npr(0:9) = (/ 0, 11, 15, 15, 15, 15, 15, 15, 15, 15 /), &
  mg21111_dnpb(0:9) = (/ 1, 11, 70, 330, 1275, 4257, 12692, 34540, 87155, &
    206305 /), &
  mg21111_npb(0:9) = (/ 1, 12, 82, 412, 1687, 5944, 18636, 53176, 140331, &
    346636 /), &
  mg21111_dnsc(0:9) = (/ 1, 0, 6, 0, 1, 0, 0, 0, 0, 0 /), &
  mg21111_nsc(0:9) = (/ 1, 1, 7, 7, 8, 8, 8, 8, 8, 8 /), &
  mg21111_dnb(0:9) = (/ 1, 11, 76, 396, 1696, 6248, 20412, 60412, 164582, &
    417802 /), &
  mg21111_nb(0:9) = (/ 1, 12, 88, 484, 2180, 8428, 28840, 89252, 253834, &
    671636 /)
!..procedures
CONTAINS
include 'mg21111_prims.f90'
include 'mg21111_prib.f90'
include 'mg21111_isecs.f90'
include 'mg21111_secs.f90'
include 'mg21111_base.f90'
END MODULE inv_mg21111
