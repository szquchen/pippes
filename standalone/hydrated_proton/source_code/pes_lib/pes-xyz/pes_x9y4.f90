MODULE pes_x9y4
!..use and access
use inv, wp=>inv_wp
use pes0
use pes_x1
use pes_y1
use pes_x2
use pes_x1y1
use pes_y2
use pes_x3
use pes_x2y1
use pes_x1y2
!! use pes_y3
use pes_x4
use pes_x3y1
use pes_x2y2
!! use pes_x1y3
use pes_x5
use pes_x4y1
use pes_x3y2
!! use pes_x2y3
!! use pes_x6
use pes_x5y1
use pes_x4y2
!! use pes_x3y3
!! use pes_x7
!! use pes_x6y1
use pes_x5y2
!! use pes_x4y3
!! use pes_x7y1
!! use pes_x6y2
!! use pes_x5y3
!! use pes_x7y2
!! use pes_x6y3
!! use pes_x7y3
private
public :: pes_x9y4_pot, pes_x9y4_getcf, pes_x9y4_vfun, pes_x9y4_getvcf
!..data
save
integer, parameter, public :: &
  pes_x9y4_nki(0:1)=(/9,4/), pes_x9y4_nk=13
character (len=*), parameter, public :: &
  pes_x9y4_sysall=pes_x5y2_sysall
!..procedures
CONTAINS
include 'pes_x9y4_pot.f90'
include 'pes_x9y4_getcf.f90'
include 'pes_x9y4_vfun.f90'
include 'pes_x9y4_getvcf.f90'
END MODULE pes_x9y4
