MODULE inv_mg422
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg422_prims, mg422_prib, mg422_isecs, mg422_secs, &
  mg422_base
!..data
integer, parameter, private :: nkk=3, nk=8, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,2,2/)
integer, parameter, public :: &
  mg422_id=53, &
  mg422_nkk=nkk, mg422_nk=nk, mg422_nr=nr, mg422_ngrp=96, &
  mg422_nkj(0:nkk-1)=nkj, &
  mg422_dnpr(0:9) = (/ 0, 6, 11, 4, 5, 0, 2, 0, 0, 0 /), &
  mg422_npr(0:9) = (/ 0, 6, 17, 21, 26, 26, 28, 28, 28, 28 /), &
  mg422_dnpb(0:9) = (/ 1, 6, 32, 126, 452, 1422, 4180, 11370, 29382, &
    71990 /), &
  mg422_npb(0:9) = (/ 1, 7, 39, 165, 617, 2039, 6219, 17589, 46971, &
    118961 /), &
  mg422_dnsc(0:9) = (/ 1, 0, 5, 38, 137, 496, 1776, 5522, 15267, 38175 /), &
  mg422_nsc(0:9) = (/ 1, 1, 6, 44, 181, 677, 2453, 7975, 23242, 61417 /), &
  mg422_dnb(0:9) = (/ 1, 6, 37, 194, 977, 4586, 20364, 84968, 333969, &
    1236943 /), &
  mg422_nb(0:9) = (/ 1, 7, 44, 238, 1215, 5801, 26165, 111133, 445102, &
    1682045 /)
!..procedures
CONTAINS
include 'mg422_prims.f90'
include 'mg422_prib.f90'
include 'mg422_isecs.f90'
include 'mg422_secs.f90'
include 'mg422_base.f90'
END MODULE inv_mg422
