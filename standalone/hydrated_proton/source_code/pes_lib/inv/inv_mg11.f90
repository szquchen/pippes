MODULE inv_mg11
!..use and access
use inv_core
use inv_mgx
implicit none
private
public :: mg11_prims, mg11_prib, mg11_secs, mg11_base
!..data
! Note: all rather trivial.  We keep it this way to be consistent
! with other routines in the inv package.
integer, private :: i_local
integer, parameter, private :: nkk=2, nk=2, nr=nk*(nk-1)/2, &
  nkj(0:nkk-1)=(/1,1/)
integer, parameter, public :: &
  mg11_id=3, &
  mg11_nkk=nkk, mg11_nk=nk, mg11_nr=nr, mg11_ngrp=1, &
  mg11_nkj(0:nkk-1)=nkj, &
  mg11_dnpr(0:19) = (/ 0, 1, (0,i_local=2,19) /), &
  mg11_npr(0:19) = (/ 0, (1,i_local=1,19) /), &
  mg11_dnpb(0:19) = (/ (1,i_local=0,19) /), &
  mg11_npb(0:19) = (/ (i_local+1,i_local=0,19) /), &
  mg11_dnsc(0:19) = (/ 1, (0,i_local=1,19) /), &
  mg11_nsc(0:19) = (/ (1,i_local=0,19) /), &
  mg11_dnb(0:19) = (/ (1,i_local=0,19) /), &
  mg11_nb(0:19) = (/ (i_local+1,i_local=0,19) /)
!..procedures
CONTAINS
include 'mg11_prims.f90'
include 'mg11_prib.f90'
include 'mg11_secs.f90'
include 'mg11_base.f90'
END MODULE inv_mg11
