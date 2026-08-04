MODULE inv_mg82
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg82_prims, mg82_prib, mg82_isecs, mg82_secs, &
  mg82_base
!..data
integer, parameter, private :: nkk=2, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/8,2/)
integer, parameter, public :: &
  mg82_id=99, &
  mg82_nkk=nkk, mg82_nk=nk, mg82_nr=nr, mg82_ngrp=, &
  mg82_nkj(0:nkk-1)=nkj, &
  mg82_dnpr(0:9) = (/ 
  mg82_npr(0:9) = (/ 
  mg82_dnpb(0:9) = (/ 
  mg82_npb(0:9) = (/ 
  mg82_dnsc(0:9) = (/ 
  mg82_nsc(0:9) = (/ 
  mg82_dnb(0:9) = (/ 
  mg82_nb(0:9) = (/ 
!..procedures
CONTAINS
include 'mg82_prims.f90'
include 'mg82_prib.f90'
include 'mg82_isecs.f90'
include 'mg82_secs.f90'
include 'mg82_base.f90'
END MODULE inv_mg82
