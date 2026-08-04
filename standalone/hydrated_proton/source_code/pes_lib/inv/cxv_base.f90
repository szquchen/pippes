SUBROUTINE cxv_base (nki, sys, pcv, r, w)
integer, intent (in) :: nki(0:)
character (len=*), intent (in) :: sys
type (cx_t), intent (in) :: pcv(2:)
real (kind=wp), intent (in) :: r(0:,0:)
real (kind=wp), intent (out) :: w(0:,0:)
!-----------------------------------------------------------------------
integer :: nkj(0:size(nki)-1), order, ib, nb, i, k, l
logical :: done
character (len=size(nki)) :: charid
if (size(r,1).ne.sum(nki).or.size(r,2).ne.sum(nki)) then
 stop 'cxv_base: bad dimension r'
else if (size(w,1).ne.sum(nki)) then
 stop 'cxv_base: bad dimension w (1)'
else if (size(w,2).ne.cxv_nbase(nki,sys,pcv)) then
 stop 'cxv_base: bad dimension w (2)'
endif
! initialize counter ib
ib = 0
! One-body terms
do i = 0, size(nki)-1
 if (1.le.nki(i)) then
  nkj = 0 ; nkj(i) = 1
  call cx_charid (nkj, charid)
  if (cx_substr(sys,charid)) then
   call cxv_b1 (nki, nkj, w(:,ib:ib))
   ib = ib+1
  endif
 endif
enddo
! Further terms grevlex
do order = 2, min(sum(nki),ubound(pcv,dim=1))
 if (0.le.pcv(order)%dg) then
  k = order
  do i = 0, size(nki)-1
   nkj(i) = min(k,nki(i))
   k = k-nkj(i)
  enddo
  done = .false.
  do while (.not.done)
   call cx_charid (nkj, charid)
   if (cx_substr(sys,charid)) then
    nb = cxv_nbx(nkj,pcv(order)%dg)
    call cxv_bx (nki, nkj, pcv(order), r, w(:,ib:ib+nb-1))
    ib = ib+nb
   endif
! set next nkj
   l = size(nki)
   do i = size(nki)-1, 0, -1
    if (nkj(i).lt.nki(i).and.sum(nkj(i:)).lt.order) then
     l = i
    endif
   enddo
   if (l.lt.size(nki)) then
    nkj(l) = nkj(l)+1
    k = order-sum(nkj(l:))
    do i = 0, l-1
     nkj(i) = min(k,nki(i))
     k = k-nkj(i)
    enddo
    done = .false.
   else
    done = .true.
   endif
  enddo
 endif
enddo
if (ib.ne.size(w,2)) then
 stop 'cxv_base: size mismatch'
endif
return
END SUBROUTINE cxv_base
