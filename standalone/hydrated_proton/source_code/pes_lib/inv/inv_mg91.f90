MODULE inv_mg91
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg91_prims, mg91_prib, mg91_isecs, mg91_secs, &
  mg91_base
!..data
integer, parameter, private :: nkk=2, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/9,1/)
integer, parameter, public :: &
  mg91_id=98, &
  mg91_nkk=nkk, mg91_nk=nk, mg91_nr=nr, mg91_ngrp=, &
  mg91_nkj(0:nkk-1)=nkj, &
  mg91_dnpr(0:9) = (/ 
  mg91_npr(0:9) = (/ 
  mg91_dnpb(0:9) = (/ 
  mg91_npb(0:9) = (/ 
  mg91_dnsc(0:9) = (/ 
  mg91_nsc(0:9) = (/ 
  mg91_dnb(0:9) = (/ 
  mg91_nb(0:9) = (/ 
!..procedures
CONTAINS
include 'mg91_prims.f90'
include 'mg91_prib.f90'
include 'mg91_isecs.f90'
include 'mg91_secs.f90'
include 'mg91_base.f90'
END MODULE inv_mg91
