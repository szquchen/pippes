MODULE inv_mg55
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg55_prims, mg55_prib, mg55_isecs, mg55_secs, &
  mg55_base
!..data
integer, parameter, private :: nkk=2, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/5,5/)
integer, parameter, public :: &
  mg55_id=102, &
  mg55_nkk=nkk, mg55_nk=nk, mg55_nr=nr, mg55_ngrp=120*120, &
  mg55_nkj(0:nkk-1)=nkj, &
  mg55_dnpr(0:9) = (/ 
  mg55_npr(0:9) = (/ 
  mg55_dnpb(0:9) = (/ 
  mg55_npb(0:9) = (/ 
  mg55_dnsc(0:9) = (/ 
  mg55_nsc(0:9) = (/ 
  mg55_dnb(0:9) = (/ 
  mg55_nb(0:9) = (/ 
!..procedures
CONTAINS
include 'mg55_prims.f90'
include 'mg55_prib.f90'
include 'mg55_isecs.f90'
include 'mg55_secs.f90'
include 'mg55_base.f90'
END MODULE inv_mg55
