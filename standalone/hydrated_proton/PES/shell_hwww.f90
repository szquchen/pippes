module shell_hwww
use constants

implicit none
!parameters in h-w-w 3b potential
real::coeff_4b(5,2)
save coeff_4b


contains

  !==================================!
  ! read the coefficients of the PES !
  !==================================!
  subroutine pes_init_hwww()
    !::::::::::::::::::
    integer::i

    open(10,file='../coef/hwww/4bfit.txt',status='old')

    do i=1,5
       read(10,*) coeff_4b(i,:)
    end do

    return
    close (10)
  end subroutine pes_init_hwww




!==================================================================
!h-w-w-w 4b potential using a simple fitting equation
!=================================================================

  function f_hwww(xh,xw) result(pot)
    implicit none
    real,dimension(:,:),intent(in)::xh,xw
    real :: pot
    real :: tmp_xw(3,3,3),r_total,sum_r(5),sum_1(6),sum_2(18,2),sum_3(18),sum_4(72),roh_h(3,3),roh_w(3,2,3),temp(4)
    real :: roh(2),tmp_roh(2),roo_w(3,3)
    real :: E4b(5),Eng
    integer::nw,cnt,i,j,k,l,m,n,flag(1)
    integer:: ii,jj,kk
    real:: s,wt(5)

    nw=size(xw,2)/3
    pot=0.d0
   

    do ii=1,nw
       do jj=ii+1,nw
           do kk=jj+1,nw   !select 3 water monomers 
              tmp_xw(:,:,1)=xw(:,3*ii-2:3*ii)
              tmp_xw(:,:,2)=xw(:,3*jj-2:3*jj)
              tmp_xw(:,:,3)=xw(:,3*kk-2:3*kk)

             !all OH bond length between hydronium and water
              do j=1,3
               do k=1,3
                 roh_h(j,k)=dsqrt(sum((xh(:,j)-tmp_xw(:,3,k))**2))
               end do
              end do
              !all OH bond length between waters 
              do j=1,3
               do k=1,2
                do l=1,3
                 roh_w(j,k,l)=dsqrt(sum((tmp_xw(:,k,j)-tmp_xw(:,3,l))**2))
                end do
               end do
              end do

             do j=1,3
                do k=1,3
                   roo_w(j,k)=dsqrt(sum((tmp_xw(:,3,j)-tmp_xw(:,3,k))**2))
                end do
             end do

             !4b energy for 3H as donor type
             cnt=0
             do j=1,3
              do k=1,3
                if (k.ne.j) then
                 cnt=cnt+1
                 sum_1(cnt)=roh_h(1,j)+roh_h(2,k)+roh_h(3,6-j-k)
                end if
              end do
             end do
             sum_r(1)=minval(sum_1)
             !4b energy for 2H as donor type 
             cnt=0
             do j=1,3
             do k=j+1,3
             do l=1,3
             do m=1,3
               if (m.ne.l) then
                  cnt=cnt+1
                  temp(1)=roh_w(l,1,6-l-m)
                  temp(2)=roh_w(l,2,6-l-m)
                  temp(3)=roh_w(6-l-m,1,l)
                  temp(4)=roh_w(6-l-m,2,l)
                  roh(1)=minval(temp)
                  temp(1)=roh_w(m,1,6-l-m)
                  temp(2)=roh_w(m,2,6-l-m)
                  temp(3)=roh_w(6-l-m,1,m)
                  temp(4)=roh_w(6-l-m,2,m)
                  roh(2)=minval(temp)        
!                  if((abs(roh(1)-roh(2)).le.(1.0/auang))) then
!                    sum_2(cnt,1)=100.d0
                    sum_2(cnt,2)=roh_h(j,l)+roh_h(k,m)+(roh(1)+roh(2))/2.0
!                  else
                    tmp_roh(1)=roh(1)
                    tmp_roh(2)=roh(2)
                    sum_2(cnt,1)=roh_h(j,l)+roh_h(k,m)+minval(tmp_roh)
!                    sum_2(cnt,2)=100.d0
!                  end if
               end if
            end do
            end do
            end do
            end do
            sum_r(2)=minval(sum_2(:,1))
            sum_r(3)=minval(sum_2(:,2))
            !4b energy for 1H as donor type with case 1
            cnt=0
            do j=1,3
            do k=1,3
            do l=1,3
             if (l.ne.k) then
              cnt=cnt+1
              sum_3(cnt)=roh_h(j,k)+roh_w(k,1,l)+roh_w(k,2,6-k-l)
             end if
            end do
            end do
            end do
            sum_r(4)=minval(sum_3)

            !4b energy for 1H as donor type with case 2
            cnt=0
            do j=1,3
            do k=1,3
            do l=1,2
            do m=1,3
              if (m.ne.k) then
                 cnt=cnt+1
                 sum_4(cnt)=roh_h(j,k)+roh_w(k,l,m)+roh_w(m,1,6-k-m)
                 !write(*,*),j,k,l,m,sum_4(cnt)
                 cnt=cnt+1
                 sum_4(cnt)=roh_h(j,k)+roh_w(k,l,m)+roh_w(m,2,6-k-m)
                 !write(*,*),j,k,l,m,sum_4(cnt)
              end if
            end do
            end do
            end do
            end do
            sum_r(5)=minval(sum_4)

            r_total=minval(sum_r)
            do j=1,5
               call f_switch_wt(wt(j),sum_r(j)-r_total)
               if (wt(j).ne.0.d0) then
                     call f_switch_4b(s,sum_r(j))
                     E4b(j)=wt(j)*(s*coeff_4b(j,1)*exp(-coeff_4b(j,2)*sum_r(j)))
               else
                     E4b(j)=0.d0
               end if
            end do 
            Eng=sum(E4b)/sum(wt)
            pot=pot+Eng
         end do
      end do
    end do

    return
        

    end function f_hwww

        
!==================================================
!switching functions for weights
!==================================================

subroutine f_switch_4b(s,r)
  real(kind=8),intent(out)::s
  real(kind=8),intent(in)::r
  real(kind=8)::ri,rf
  !::::::::::::::::::::
  real(kind=8)::ra,ra2,ra3


!  ri(1:5)=4.5/auang
   ri=4.5/auang
   rf=6.0/auang
!  rf(1:5)=5.5/auang

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

end subroutine f_switch_4b


  subroutine f_switch_wt(s,r)
    real :: s,r,x
    real :: rf1,rf2
    
    rf1=0.0/auang
    rf2=2.0/auang

    x=(r-rf1)/(rf2-rf1)

    if (r.le.rf1) then
       s=1.0
    elseif (r.le.rf2) then
       s=1-6*x**5+15*x**4-10*x**3
    else
       s=0.0
    end if

    return
  end subroutine f_switch_wt


end module shell_hwww
