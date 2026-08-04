MODULE inv_mg2
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg2_prims, mg2_prib, mg2_secs, mg2_base
!..data
! Note: all rather trivial.  We keep it this way to be consistent
! with other routines in the inv package.
integer, private :: i_local
integer, parameter, private :: nkk=1, nk=2, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/2/)
integer, parameter, public :: &
  mg2_id=2, &
  mg2_nkk=nkk, mg2_nk=nk, mg2_nr=nr, mg2_ngrp=2, &
  mg2_nkj(0:nkk-1)=nkj, &
  mg2_dnpr(0:19) = (/ 0, 1, (0,i_local=2,19) /), &
  mg2_npr(0:19) = (/ 0, (1,i_local=1,19) /), &
  mg2_dnpb(0:19) = (/ (1,i_local=0,19) /), &
  mg2_npb(0:19) = (/ (i_local+1,i_local=0,19) /), &
  mg2_dnsc(0:19) = (/ 1, (0,i_local=1,19) /), &
  mg2_nsc(0:19) = (/ (1,i_local=0,19) /), &
  mg2_dnb(0:19) = (/ (1,i_local=0,19) /), &
  mg2_nb(0:19) = (/ (i_local+1,i_local=0,19) /)
!..procedures
CONTAINS
include 'mg2_prims.f90'
include 'mg2_prib.f90'
include 'mg2_secs.f90'
include 'mg2_base.f90'
END MODULE inv_mg2
