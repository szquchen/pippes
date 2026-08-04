MODULE inv_mg64
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg64_prims, mg64_prib, mg64_isecs, mg64_secs, &
  mg64_base
!..data
integer, parameter, private :: nkk=2, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/6,4/)
integer, parameter, public :: &
  mg64_id=101, &
  mg64_nkk=nkk, mg64_nk=nk, mg64_nr=nr, mg64_ngrp=720*24, &
  mg64_nkj(0:nkk-1)=nkj, &
  mg64_dnpr(0:9) = (/ 
  mg64_npr(0:9) = (/ 
  mg64_dnpb(0:9) = (/ 
  mg64_npb(0:9) = (/ 
  mg64_dnsc(0:9) = (/ 
  mg64_nsc(0:9) = (/ 
  mg64_dnb(0:9) = (/ 
  mg64_nb(0:9) = (/ 
!..procedures
CONTAINS
include 'mg64_prims.f90'
include 'mg64_prib.f90'
include 'mg64_isecs.f90'
include 'mg64_secs.f90'
include 'mg64_base.f90'
END MODULE inv_mg64
