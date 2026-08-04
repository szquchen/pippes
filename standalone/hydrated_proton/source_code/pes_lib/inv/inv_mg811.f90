MODULE inv_mg811
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg811_prims, mg811_prib, mg811_isecs, mg811_secs, &
  mg811_base
!..data
integer, parameter, private :: nkk=3, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/8,1,1/)
integer, parameter, public :: &
  mg811_id=103, &
  mg811_nkk=nkk, mg811_nk=nk, mg811_nr=nr, mg811_ngrp=40320, &
  mg811_nkj(0:nkk-1)=nkj, &
  mg811_dnpr(0:9) = (/ 
  mg811_npr(0:9) = (/ 
  mg811_dnpb(0:9) = (/ 
  mg811_npb(0:9) = (/ 
  mg811_dnsc(0:9) = (/ 
  mg811_nsc(0:9) = (/ 
  mg811_dnb(0:9) = (/ 
  mg811_nb(0:9) = (/ 
!..procedures
CONTAINS
include 'mg811_prims.f90'
include 'mg811_prib.f90'
include 'mg811_isecs.f90'
include 'mg811_secs.f90'
include 'mg811_base.f90'
END MODULE inv_mg811
