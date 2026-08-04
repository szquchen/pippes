FUNCTION pes_x6_pot (xn) RESULT (f)
! Potential for generic X6
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x6_nk-1,0:pes_x6_nk-1)
integer, parameter :: nki(0:0)=pes_x6_nki
call pes_dists (xn, r)
f = pes_x1_cf*nki(0)+ &
  cx_f2(nki,r,pes_x2_pc,pes_x2_cf)+ &
  cx_f3(nki,r,pes_x3_pc,pes_x3_cf)+ &
  cx_f4(nki,r,pes_x4_pc,pes_x4_cf)+ &
  cx_f5(nki,r,pes_x5_pc,pes_x5_cf)+ &
  cx_f6(nki,r,pes_x6_pc,pes_x6_cf)
return
END FUNCTION pes_x6_pot
