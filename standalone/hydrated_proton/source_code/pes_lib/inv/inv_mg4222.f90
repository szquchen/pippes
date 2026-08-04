MODULE inv_mg4222
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg4222_prims, mg4222_prib, mg4222_isecs, mg4222_secs, &
  mg4222_base
!..data
integer, parameter, private :: nkk=4, nk=10, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,2,2,2/)
integer, parameter, public :: &
  mg4222_id=118, &
  mg4222_nkk=nkk, mg4222_nk=nk, mg4222_nr=nr, mg4222_ngrp=24*2*2*2, &
  mg4222_nkj(0:nkk-1)=nkj, &
  mg4222_dnpr(0:9) = (/ 0, 10, 20, 5, 7, 0, 3, 0, 0, 0 /), &
  mg4222_npr(0:9) = (/ 0, 10, 30, 35, 42, 42, 45, 45, 45, 45 /), &
  mg4222_dnpb(0:9) = (/ 1, 10, 75, 425, 2082, 8947, 35038, 126360, &
    426528, 1357194 /), &
  mg4222_npb(0:9) = (/ 1, 11, 86, 511, 2593, 11540, 46578, 172938, &
    599466, 1956660 /), &
  mg4222_dnsc(0:9) = (/ 1, 0, 15, 108, 541, 2968, 15198, 68770, &
    282859, 1060615 /), &
  mg4222_nsc(0:9) = (/ 1, 1, 16, 124, 665, 3633, 18831, 87601, &
    370460, 1431075 /), &
  mg4222_dnb(0:9) = (/ 1, 10, 90, 683, 4828, 31800, 197621, 1158696, &
    6416545, 33562506 /), &
  mg4222_nb(0:9) = (/ 1, 11, 101, 784, 5612, 37412, 235033, 1393729, &
    7810274, 41372780 /)
!..procedures
CONTAINS
include 'mg4222_prims.f90'
include 'mg4222_prib.f90'
include 'mg4222_isecs.f90'
include 'mg4222_secs.f90'
include 'mg4222_base.f90'
END MODULE inv_mg4222
