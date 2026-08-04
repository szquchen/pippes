MODULE inv_mg1
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg1_prims, mg1_prib, mg1_secs, mg1_base
!..data
! Note: all totally trivial.  We keep it this way to be consistent
! with other routines in the inv package.
integer, private :: i_local
integer, parameter, private :: nkk=1, nk=1, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/1/)
integer, parameter, public :: &
  mg1_id=1, &
  mg1_nkk=nkk, mg1_nk=nk, mg1_nr=nr, mg1_ngrp=1, &
  mg1_nkj(0:nkk-1)=nkj, &
  mg1_dnpr(0:19) = 0, &
  mg1_npr(0:19) = 0, &
  mg1_dnpb(0:19) = (/ 1, (0,i_local=1,19) /), &
  mg1_npb(0:19) = 1, &
  mg1_dnsc(0:19) = (/ 1, (0,i_local=1,19) /), &
  mg1_nsc(0:19) = 1, &
  mg1_dnb(0:19) = (/ 1, (0,i_local=1,19) /), &
  mg1_nb(0:19) = 1
!..procedures
CONTAINS
include 'mg1_prims.f90'
include 'mg1_prib.f90'
include 'mg1_secs.f90'
include 'mg1_base.f90'
END MODULE inv_mg1
