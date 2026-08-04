module shell_hw
  use constants
  use shell_h
  implicit none

contains
!  include 'h5o2.pes4B-dms4B.f'
  !=================================================!
  ! H5O2+ PES initialization wrapper                !
  !=================================================!
  subroutine pes_init_hw()
    call prepot_z('../coef/h5o2+/h5o2.pes4B.coeff.dat')
    call predip_z('../coef/h5o2+/h5o2.dms4B.coeff.com.dat')
    return
  end subroutine pes_init_hw


  function f_hw(xh,xw) result(pot)
    implicit none
    real,dimension(:,:),intent(in)::xh,xw
    ! ::::::::::::::::::::
    real,dimension(7,3)::x
    integer::ih(4),iw(3)
    integer::i,nw
    real::pot,pot2b

    ! x(7,3) is an array for Xinchuan's zundel PES, in the order of OOHHHHH
    ih = (/3,4,5,1/)  ! the indices of HHHO
    iw = (/6,7,2/)    ! the indices of HHO
    nw = size(xw,2) / 3 

    pot = 0.d0
    do i=1,nw
      x(1,:) = xh(:,4) 
      x(2,:) = xw(:,i*3)
      x(3,:) = xh(:,1)
      x(4,:) = xh(:,2)
      x(5,:) = xh(:,3)
      x(6,:) = xw(:,i*3-2)
      x(7,:) = xw(:,i*3-1)
      pot2b = pot2bhw(x,ih,iw)
      pot = pot + pot2b
    enddo

    return
  end function f_hw


  function pot2bhw(x,index_h,index_w) result(pot2b)
    implicit none

    real,dimension(7,3)::x
    integer::index_h(4),index_w(3) !array of atomic index in a fragment
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real,dimension(7,3)::x_far
    real::pot,pot_far,pot2b,vec(3)
    real::rij(4,3),xh(3,4),xw(3,3),pot_chg,chg_hy(4),chg_w(3),dp_w(3)
    real::minr,s
    integer::i,j

    xh(:,1:4)=transpose(x(index_h(1:4),:))
    xw(:,1:3)=transpose(x(index_w(1:3),:))
    do i=1,4
       do j=1,3
          rij(i,j)=dsqrt(sum((xh(:,i)-xw(:,j))**2))
       end do
    end do
    minr=minval(rij)
    call f_switch_hw(s,minr)
    if(s.eq.1.0) then
        pot2b=0.d0
        call calcpot_z(pot,x)
        vec=x(index_w(3),:)-x(index_h(4),:)
        vec=vec*100.d0
        x_far=x
        do i=1,3
           x_far(index_w(i),:)=x_far(index_w(i),:)+vec(:)
        end do
        call calcpot_z(pot_far,x_far)
        pot2b=pot-pot_far
    else if (s.eq.0.0) then
         pot2b=0.d0
         pot_chg=0.d0
         chg_hy=chg_h(xh)
         call dip_ltp2011(xw,dp_w,chg_w)

         do i=1,4
           do j=1,3
              pot_chg=pot_chg+chg_hy(i)*chg_w(j)/rij(i,j)
           end do
         end do
         pot2b=pot_chg
    else
        pot2b=0.d0
        call calcpot_z(pot,x)
        vec=x(index_w(3),:)-x(index_h(4),:)
        vec=vec*100.d0
        x_far=x
        do i=1,3
           x_far(index_w(i),:)=x_far(index_w(i),:)+vec(:)
        end do
        call calcpot_z(pot_far,x_far)
        pot2b=pot-pot_far

         pot_chg=0.d0
         chg_hy=chg_h(xh)
         call dip_ltp2011(xw,dp_w,chg_w)

         do i=1,4
           do j=1,3
              pot_chg=pot_chg+chg_hy(i)*chg_w(j)/rij(i,j)
           end do
         end do
         pot2b=s*pot2b+(1-s)*pot_chg
      end if
    return

  end function pot2bhw

  function dip2bhw(xh,xw) result(dp)
    implicit none  
    real,dimension(7,3)::x,x_far
    real,dimension(3,7)::xx
    real,dimension(:,:),intent(in)::xh,xw
    integer :: ih(4),iw(3)
    integer ::i,j
    real::chg2b(7),vec(3),chg1(7),chg2(7),dp(3)

    x(1,:)=xh(:,4)
    x(2,:)=xw(:,3)
    x(3,:)=xh(:,1)
    x(4,:)=xh(:,2)
    x(5,:)=xh(:,3)
    x(6,:)=xw(:,1)
    x(7,:)=xw(:,2)
    
    call calcdip_z(dp,chg1,x)
    vec=x(2,:)-x(1,:)
    vec=vec*100.d0
    x_far=x
    x_far(2,:)=x_far(2,:)+vec(:)
    x_far(6,:)=x_far(6,:)+vec(:)
    x_far(7,:)=x_far(7,:)+vec(:)
    call calcdip_z(dp,chg2,x_far)
    chg2b=chg1-chg2
    xx=transpose(x)
    dp=matmul(xx,chg2b)
end function dip2bhw   

  subroutine f_switch_hw(s,r)
  real(kind=8),intent(out)::s
  real(kind=8),intent(in)::r
  real(kind=8)::ri,rf
  !::::::::::::::::::::
  real(kind=8)::ra,ra2,ra3

  ri=5.0/auang
  rf=7.0/auang

 if (r.lt.ri) then
    s=1.0
  else
     if (r.le.rf) then
        ra=(r-ri)/(rf-ri)
        ra2=ra*ra
        ra3=ra2*ra
        s=10.0*ra3-15.0*ra*ra3+6.0*ra3*ra2
        s=1-s
     else
        s=0.0
     end if
  end if
 return

end subroutine f_switch_hw

end module shell_hw
