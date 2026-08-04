module pes_shell
  use constants
  use shell_h ! hydronium 1-body
  use shell_w ! water 1b, 2b and 3b potentials.
  use shell_hw ! hydronium and water 2-body interaction
  use shell_hww ! hydronium water water 3-body 
  use shell_hwww

  ! define global variables
  integer::nw ! number of water molecules
  real,parameter :: rf1=-0.5,rf2=0.5 !shift range in monomer assignment 
contains
  !=====================!
  ! Initialize the PES  !
  !=====================!
  subroutine pes_init(nh,nw)

    call pes_init_h()
    call pes_init_w(nw) 
    call pes_init_hw()
    call pes_init_hww()
    call pes_init_hwww()
  end subroutine


  !===========================!
  ! Evaluating the potential  !
  !===========================!
  function f(x)
    real,dimension(:,:),intent(in) :: x !cartesian coordinate in bohr,x(3,natm)
    ! ::::::::::::::::::::
    real::f,eng(3) !potential of whole system, pivot parts,non-pivot parts

    real::temp_x(3,size(x,2),3),xx(3,size(x,2)),s(6)
    integer :: i,j,cnt
    


    !x(3,natm)  !arrangement as HHHOHHO...HHO
    eng=0.d0
    temp_x=0
    xx=x

   !monomer assignment 
    call monomer_assignment(xx) 
     
   !calculate the energy of pivot hydronium assignment
   call pot_add(xx,f)

   !calculate the weights of non-pivot hydronium assignment
    call cal_swit(xx,s,temp_x)

   !calculate the energy of non-pivot assignments
    if(nw.lt.3) then
      cnt=nw
    else
      cnt=3
    end if

    do i=1,cnt
       if (s(2*i).eq.0.0) then
          eng(i)=0.d0
       else
          call pot_add(temp_x(:,:,i),eng(i))
       end if
    end do

   !calculate the final potential
   if(cnt==3) then
       f=f*s(1)*s(3)*s(5)+eng(1)*s(2)*s(3)*s(5)+eng(2)*s(1)*s(4)*s(5)+eng(3)*s(1)*s(3)*s(6)
       f=f/(s(1)*s(3)*s(5)+s(2)*s(3)*s(5)+s(4)*s(1)*s(5)+s(1)*s(3)*s(6))
    else
       if(cnt==2) then
         f=f*s(1)*s(3)+eng(1)*s(2)*s(3)+eng(2)*s(1)*s(4)
         f=f/(s(1)*s(3)+s(2)*s(3)+s(4)*s(1))
       else
           f=f*s(1)+eng(1)*s(2)
           f=f/(s(1)+s(2))
       end if
    end if
    return
  end function

  !===============================!
  ! Evaluating the dipole moment  !
  !===============================!
  function dp(x)
    real,dimension(:,:),intent(in) :: x !cartesian coordinate in bohr,x(3,natm)
    ! ::::::::::::::::::::
    real::dp(3),dmp(3,3) !dipole of whole system, pivot parts,non-pivot parts

    real::temp_x(3,size(x,2),3),xx(3,size(x,2)),s(6)
    integer :: i,j,cnt


    !x(3,natm)  !arrangement as HHHOHHO...HHO
    dmp=0.d0
    temp_x=0
    xx=x

   !monomer assignment 
    call monomer_assignment(xx)

   !calculate the dipole moment of pivot hydronium assignment
   dp=dip_t(xx)

   !calculate the weights of non-pivot hydronium assignment
   call cal_swit(xx,s,temp_x)

   !calculate the energy of non-pivot assignments
    if(nw.lt.3) then
      cnt=nw
    else
      cnt=3
    end if

    do i=1,cnt
       if (s(2*i).eq.0.0) then
          dmp(:,i)=0.d0
       else
          dmp(:,i)=dip_t(temp_x(:,:,i))
       end if
    end do


   !calculate the final dipole moment
   if(cnt==3) then
       dp=dp*s(1)*s(3)*s(5)+dmp(:,1)*s(2)*s(3)*s(5)+dmp(:,2)*s(1)*s(4)*s(5)+dmp(:,3)*s(1)*s(3)*s(6)
       dp=dp/(s(1)*s(3)*s(5)+s(2)*s(3)*s(5)+s(4)*s(1)*s(5)+s(1)*s(3)*s(6))
    else
       if(cnt==2) then
         dp=dp*s(1)*s(3)+dmp(:,1)*s(2)*s(3)+dmp(:,2)*s(1)*s(4)
         dp=dp/(s(1)*s(3)+s(2)*s(3)+s(4)*s(1))
       else
           dp=dp*s(1)+dmp(:,1)*s(2)
           dp=dp/(s(1)+s(2))
       end if
    end if
    return
  end function

subroutine monomer_assignment(x)
   implicit none
   real,dimension(:,:),intent(inout)::x

    real::temp_x(3,size(x,2)),xx(3,size(x,2))
    real::sumr(nw+1),rroh(3),rroh_1(2),notr(nw),minoh,min_sum,notp !variable used in determine pivot hydronium core 
    real::roo(nw),minoo,notw(3) !variable used in determine first-hydration shell

    integer :: i,j,cnt
    
    sumr=0.d0
    temp_x=x
    xx=x
 
    !determine the pivot hydronium core
    do i=1,3
    sumr(1)=sumr(1)+sqrt(sum((x(:,4)-x(:,i))**2))
    end do
    do i=1,nw
       do j=1,3
          rroh(j)=sqrt(sum((x(:,j)-x(:,4+3*i))**2))
       end do
       minoh=minval(rroh)
       do j=1,2
       rroh_1(j)=sqrt(sum((x(:,4+3*i-j)-x(:,4+3*i))**2))
       end do
       sumr(1+i)=minoh+rroh_1(1)+rroh_1(2)
       do j=1,3
          if(rroh(j).eq.minoh) then
             notr(i)=j
             exit
           end if
       end do
    end do
    min_sum=minval(sumr)
    do i=1,1+nw
        if(sumr(i).eq.min_sum) then
          notp=i
          exit
        end if
    end do
    if (notp.ne.1) then
       temp_x(:,1:2)=x(:,3*notp-1:3*notp)
       temp_x(:,3)=x(:,notr(notp-1))
       temp_x(:,4)=x(:,3*notp+1)
       temp_x(:,3*notp+1)=x(:,4)
       cnt=1
       do i=1,3
          if(i.ne.notr(notp-1)) then
            temp_x(:,3*notp-cnt)=x(:,i)
            cnt=cnt-1
          end if
       end do
    end if
    
    xx=temp_x
    !determine first hydration shell 
    if(nw.gt.3) then
    do i=1,nw
       roo(i)=sqrt(sum((xx(:,4)-xx(:,4+3*i))**2))
    end do
    cnt=1
    do while (cnt.lt.4)
       minoo=minval(roo)
       do i=1,nw
          if(roo(i)==minoo) then
            notw(cnt)=i
            roo(i)=100
            cnt=cnt+1
            exit
          end if
       end do
    end do

    do i=1,3
       temp_x(:,4+3*i-2:4+3*i)=xx(:,4+3*notw(i)-2:4+3*notw(i))
    end do
    cnt=1
    do i=1,nw
       if((i.ne.notw(1)).and.(i.ne.notw(2)).and.(i.ne.notw(3))) then
          temp_x(:,13+3*cnt-2:13+3*cnt)=xx(:,4+3*i-2:4+3*i)
          cnt=cnt+1
       end if
    end do
    end if
    x=temp_x
    return
 end subroutine


 subroutine cal_swit(x,s,temp_x)
   implicit none
   real,dimension(:,:),intent(in)::x
   real::s(6),ss,tmpr(3),roh(6),temp_x(3,size(x,2),3),eng(3),dif_r
   integer :: i,j,not_w,cnt,nm_shell_w
  
    s=0.d0
    eng=0.d0
    roh=0.d0
    tmpr=0.d0

    !calculate the oh distance of hydronium core
    do i=1,3
       roh(i)=sqrt(sum((x(:,i)-x(:,4))**2))
    end do
    if(nw.lt.3) then
       nm_shell_w=nw
    else
       nm_shell_w=3
    end if
    
    !calculate the switching coeff for each "hydronium" 
    do i=1,nm_shell_w
        do j=1,3
           tmpr(j)=sqrt(sum((x(:,j)-x(:,4+3*i))**2))
        end do
        roh(3+i)=minval(tmpr)
        do j=1,3
           if (tmpr(j)==roh(3+i)) then
              not_w=j
              exit
           end if
        end do
        cnt=0
        temp_x(:,:,i)=x
        temp_x(:,1,i)=x(:,not_w)
        temp_x(:,2:4,i)=x(:,4+3*i-2:4+3*i)
        temp_x(:,4+3*i,i)=x(:,4)

        
        do j=1,3
           if(j.ne.not_w) then
             temp_x(:,4+3*i-2+cnt,i)=x(:,j)
             cnt=cnt+1
           end if
        end do
        dif_r=roh(not_w)-roh(3+i)
        dif_r=dif_r*auang
        call f_switch_b(ss,dif_r)
        s(2*i-1)=ss
        s(2*i)=1-ss
    end do
    return
 end subroutine cal_swit


 subroutine pot_add(x,pot)
   real,dimension(:,:),intent(in):: x 
   real,dimension(3,4)::xh
   real,dimension(3,nw*3)::xw
   real,dimension(nw*9)::xw_whbb
   real::pot,pot_w,pot_h,pot_hw,pot_hww,pot_hwww
   

   xh=x(:,1:4)
   xw=x(:,5:)
   do i=1,nw
      xw_whbb(i*6-5:i*6-3)=xw(:,i*3-2)
      xw_whbb(i*6-2:i*6)=xw(:,i*3-1)
      xw_whbb(nw*6+i*3-2:nw*6+i*3)=xw(:,i*3)
   end do
   pot_h=0.d0
   pot_w=0.d0
   pot_hw=0.d0
   pot_hww=0.d0
   pot_hwww=0.d0

   pot_h=f_h(xh)
   pot_hw=f_hw(xh,xw)
   pot_w=f_w(xw_whbb)
   pot_hww=f_hww(xh,xw)
   pot_hwww=f_hwww(xh,xw)  

   pot=pot_w+pot_h+pot_hw+pot_hww+pot_hwww
   return
 end subroutine pot_add

  function dip_t(x)
  real,dimension(:,:),intent(in) :: x
  real:: dip_t(3),dp_h(3),dp_hw(3),dp_w(3)
  real,dimension(3,4)::xh
  real,dimension(3,nw*3)::xw
  real,dimension(nw*9) :: xw_whbb
  integer :: i

  xh=x(:,1:4)
  xw=x(:,5:)
  do i=1,nw
      xw_whbb(i*6-5:i*6-3) = xw(:,i*3-2)
      xw_whbb(i*6-2:i*6) = xw(:,i*3-1)
      xw_whbb(nw*6+i*3-2:nw*6+i*3) = xw(:,i*3)
  enddo
  dp_h=0.d0
  dp_hw=0.d0
  dp_w=0.d0

  dp_h=di_h(xh)

  do i=1,nw
  dp_hw=dp_hw+dip2bhw(xh,xw(:,3*i-2:3*i))
  end do

  call dipole_w(xw_whbb,dp_w)
  dip_t=dp_h+dp_hw+dp_w
  return
  end function

 subroutine f_switch_b(s,r)
   implicit none
   real :: s,r,x
   
   x=(r-rf1)/(rf2-rf1)

   if(r.le.rf1) then
      s=1.0
   else
      if(r.le.rf2) then
         s=1-6*x**5+15*x**4-10*x**3       
      else
        s=0.0
      end if

    end if

   return
 end subroutine f_switch_b
end module pes_shell
