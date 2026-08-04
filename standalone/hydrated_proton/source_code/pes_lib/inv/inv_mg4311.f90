MODULE inv_mg4311
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg4311_prims, mg4311_prib, mg4311_isecs, mg4311_secs, &
  mg4311_base
!..data
integer, parameter, private :: nkk=4, nk=9, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/4,3,1,1/)
integer, parameter, public :: &
  mg4311_id=81, &
  mg4311_nkk=nkk, mg4311_nk=nk, mg4311_nr=nr, mg4311_ngrp=144, &
  mg4311_nkj(0:nkk-1)=nkj, &
  mg4311_dnpr(0:9) = (/ 0, 8, 10, 10, 6, 0, 1, 0, 0, 0 /), &
  mg4311_npr(0:9) = (/ 0, 8, 18, 28, 34, 34, 35, 35, 35, 35 /), &
  mg4311_dnpb(0:9) = (/ 1, 8, 46, 210, 831, 2940, 9548, 28870, 82247, &
    222550 /), &
  mg4311_npb(0:9) = (/ 1, 9, 55, 265, 1096, 4036, 13584, 42454, 124701, &
    347251 /), &
  mg4311_dnsc(0:9) = (/ 1, 0, 12, 67, 302, 1432, 6284, 24403, 87007, &
    282787 /), &
  mg4311_nsc(0:9) = (/ 1, 1, 13, 80, 382, 1814, 8098, 32501, 119508, &
    402295 /), &
  mg4311_dnb(0:9) = (/ 1, 8, 58, 373, 2221, 12390, 65222, 323794, 1516780, &
    6707599 /), &
  mg4311_nb(0:9) = (/ 1, 9, 67, 440, 2661, 15051, 80273, 404067, 1920847, &
    8628446 /)
! Note, there is also a primary at degree 12
!..procedures
CONTAINS
include 'mg4311_prims.f90'
include 'mg4311_prib.f90'
include 'mg4311_isecs.f90'
include 'mg4311_secs.f90'
include 'mg4311_base.f90'
END MODULE inv_mg4311
