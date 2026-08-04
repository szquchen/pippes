SUBROUTINE cxv_getcf (proc, nki, sys, pcv, cf)
! Extract the separate coefficient blocks from array cf.
interface
 subroutine proc (nki, nkj, pc, cf)
 use inv_wp
 use inv_cxx
 integer, intent (in) :: nki(0:), nkj(0:)
 type (cx_t), intent (in) :: pc
 real (kind=wp), intent (in) :: cf(0:)
 end subroutine proc
end interface
integer, intent (in) :: nki(0:)
character (len=*), intent (in) :: sys
type (cx_t), intent (in) :: pcv(2:)
real (kind=wp), intent (in) :: cf(0:)
!-----------------------------------------------------------------------
integer :: l0, nkj(0:size(nki)-1), order, ib, nb, i, k, l
logical :: done
character (len=size(nki)) :: charid
if (size(cf).ne.cxv_nbase(nki,sys,pcv)) then
 stop 'cxv_getcf: bad dimension cf'
endif
l0 = len_trim(sys)
! initialize counter ib
ib = 0
! One-body terms
do i = 0, size(nki)-1
 if (1.le.nki(i)) then
  nkj = 0 ; nkj(i) = 1
  call cx_charid (nkj, charid)
  if (cx_substr(sys(1:l0),charid)) then
   nb = 1
   call proc (nki, nkj, cx_null, cf(ib:ib+nb-1))
   ib = ib+nb
  endif
 endif
enddo
! Further terms grevlex
do order = 2, min(sum(nki),ubound(pcv,dim=1))
 k = order
 do i = 0, size(nki)-1
  nkj(i) = min(k,nki(i))
  k = k-nkj(i)
 enddo
 done = .false.
 do while (.not.done)
  call cx_charid (nkj, charid)
  if (cx_substr(sys(1:l0),charid)) then
   nb = cxv_nbx(nkj,pcv(order)%dg)
   call proc (nki, nkj, pcv(order), cf(ib:ib+nb-1))
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
enddo
if (ib.ne.size(cf)) then
 stop 'cxv_getcf: size mismatch'
endif
return
END SUBROUTINE cxv_getcf
