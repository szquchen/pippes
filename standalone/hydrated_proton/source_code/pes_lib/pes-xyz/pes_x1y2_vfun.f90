FUNCTION pes_x1y2_vfun (xn) RESULT (f)
! Vector function for generic X1Y2
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
real (kind=wp) :: r(0:pes_x1y2_nk-1,0:pes_x1y2_nk-1)
integer, parameter :: nki(0:1)=pes_x1y2_nki
call pes_dists (xn, r)
! Dissociates into X1+Y2 and X1Y1+Y1
f = cxv_f1(nki,pes_x1_vcf)+ &
  cxv_f01(nki,pes_y1_vcf)+ &
  cxv_f11(nki,r,pes_x1y1_vpc,pes_x1y1_vcf)+ &
  cxv_f02(nki,r,pes_y2_vpc,pes_y2_vcf)+ &
  cxv_f12(nki,r,pes_x1y2_vpc,pes_x1y2_vcf)
return
END FUNCTION pes_x1y2_vfun
