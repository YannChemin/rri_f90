! RRI_Dam.f90

! reading dam control file
subroutine dam_read
use globals
use dam_mod
implicit none
    
integer :: i
    
allocate( damflg(riv_count), dam_qin(riv_count) )
damflg(:) = 0

if( dam_switch.eq.1 ) then

 open(99, file=Damfile, status="old" )
 read(99,*) dam_num
 allocate( dam_name(dam_num), dam_kind(dam_num) &
           , dam_ix(dam_num), dam_iy(dam_num) &
           , dam_vol(dam_num), dam_vol_temp(dam_num) &
           , dam_volmax(dam_num), dam_state(dam_num) &
           , dam_qout(dam_num), dam_loc(dam_num) &
           , dam_floodq(dam_num) )

 dam_vol(:) = 0.d0
 dam_state(:) = 0

 do i = 1, dam_num
  read(99,*) dam_name(i), dam_iy(i), dam_ix(i), dam_volmax(i), dam_floodq(i)
  dam_loc(i) = riv_ij2idx( dam_iy(i), dam_ix(i) )
  damflg(dam_loc(i)) = i
 end do     
 close(99)
end if
end subroutine dam_read

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! calculating inflow to dam
subroutine dam_prepare(qr_idx)

use globals
!use dam_mod, only :dam_qin
use dam_mod
implicit none

integer :: i, k, kk
real(8) :: qr_idx(riv_count), vr_idx(riv_count)

!dam_qin(:) = 0.d0
!do k = 1, riv_count
! kk = down_riv_idx(k)
! dam_qin(kk) = dam_qin(kk) + qr_idx(k)
!enddo

! modified by TS on June 16, 2016
dam_qin(:) = qr_idx(:)

end subroutine dam_prepare

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine dam_operation(k)

!use globals, only :ddt,area
use globals, only :ddt ! v1.4
use dam_mod
implicit none

integer :: k
real(8) :: qdiff
    
dam_qout(damflg(k)) = 0.d0

!if ( dam_qin(k) * area .lt. dam_floodq(damflg(k)) ) then 
if ( dam_qin(k) .lt. dam_floodq(damflg(k)) ) then ! v1.4
 dam_qout(damflg(k)) = dam_qin(k)
else
 if (dam_state(damflg(k)) .eq. 0) then
 ! still have space
  !dam_qout(damflg(k)) = dam_floodq(damflg(k)) / area
  dam_qout(damflg(k)) = dam_floodq(damflg(k)) ! v1.4
  !qdiff = (dam_qin(k) - dam_qout(damflg(k))) * area
  qdiff = (dam_qin(k) - dam_qout(damflg(k))) ! v1.4
  dam_vol_temp(damflg(k)) = dam_vol_temp(damflg(k)) + qdiff * ddt
 else
  ! no more space
  dam_qout(damflg(k)) = dam_qin(k)
 end if
end if

end subroutine dam_operation

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine dam_checkstate(qr_ave_idx)

use globals,only :riv_count,area
use dam_mod
implicit none
    
real(8) :: qr_ave_idx(riv_count)
integer :: i
    
do i=1, dam_num
 dam_vol_temp(i) = dam_vol_temp(i) / 6.d0
 dam_vol(i) = dam_vol(i) + dam_vol_temp(i)
 if (dam_vol(i) .gt. dam_volmax(i)) dam_state(i) = 1
end do
    
end subroutine dam_checkstate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine dam_write

!use globals, only :area
use dam_mod
implicit none
integer i

!write(1001,'(100f20.5)') (dam_vol(i), dam_qin(dam_loc(i)) * area, dam_qout(i) * area, i = 1, dam_num) 
write(1001,'(100f20.5)') (dam_vol(i), dam_qin(dam_loc(i)), dam_qout(i), i = 1, dam_num) ! v1.4
    
end subroutine dam_write
