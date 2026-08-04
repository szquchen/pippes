FUNCTION pes_y2_vfun (xn) RESULT (f)
! Vector function for generic Y2
real (kind=wp), intent (in) :: xn(0:,0:)
real (kind=wp) :: f(0:size(xn,2)-1)
!-----------------------------------------------------------------------
integer, parameter :: nki(0:0)=pes_y2_nki
real (kind=wp) :: r(0:pes_y2_nk-1,0:pes_y2_nk-1)
call pes_dists (xn, r)
f = cxv_f1(nki,pes_y1_vcf)+ &
  cxv_f2(nki,r,pes_y2_vpc,pes_y2_vcf)
return
END FUNCTION pes_y2_vfun
