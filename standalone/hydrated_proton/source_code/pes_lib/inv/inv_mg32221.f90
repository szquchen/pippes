MODULE inv_mg32221
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg32221_prims, mg32221_prib, mg32221_isecs, mg32221_secs, &
  mg32221_base
!..data
integer, parameter, private :: nkk=5, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/3,2,2,2,1/)
integer, parameter, public :: &
  mg32221_id=125, &
  mg32221_nkk=nkk, mg32221_nk=nk, mg32221_nr=nr, mg32221_ngrp=6*8, &
  mg32221_nkj(0:nkk-1)=nkj, &
  mg32221_dnpr(0:9) = (/ 0, 14, 23, 5, 0, 0, 3, 0, 0, 0 /), &
  mg32221_npr(0:9) = (/ 0, 14, 37, 42, 42, 42, 45, 45, 45, 45 /), &
  mg32221_dnpb(0:9) = (/ 1, 14, 128, 887, 5141, 25952, 117580, &
    486951, 1869720, 6724134 /), &
  mg32221_npb(0:9) = (/ 1, 15, 143, 1030, 6171, 32123, 149703, &
    636654, 2506374, 9230508 /), &
  mg32221_dnsc(0:9) = (/ 1, 0, 28, 157, 844, 4584, 21034, &
    82659, 288788, 891768 /), &
  mg32221_nsc(0:9) = (/ 1, 1, 29, 186, 1030, 5614, 26648, &
    109307, 398095, 1289863 /), &
  mg32221_dnb(0:9) = (/ 1, 14, 156, 1436, 11767, 87284, 594029, &
    3733259, 21779802, 118460964 /), &
  mg32221_nb(0:9) = (/ 1, 15, 171, 1607, 13374, 100658, 694687, &
    4427946, 26207748, 144668712 /)
!..procedures
CONTAINS
include 'mg32221_prims.f90'
include 'mg32221_prib.f90'
include 'mg32221_isecs.f90'
include 'mg32221_secs.f90'
include 'mg32221_base.f90'
END MODULE inv_mg32221
