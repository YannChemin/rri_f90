! RRI_Riv.f90

! variable definition (river)

subroutine funcr( hr_idx, fr_idx, qr_idx )
use globals
use dam_mod, only: dam_switch, damflg, dam_qout, dam_volmax, dam_floodq, dam_num, dam_loc
use omp_lib
implicit none

real(8) hr_idx(riv_count), fr_idx(riv_count), qr_idx(riv_count)

real(8) qr_sum_idx(riv_count), qr_div_idx(riv_count)
integer i, k, kk, itemp, jtemp , l
integer kk_div

fr_idx(:) = 0.d0
qr_idx(:) = 0.d0
qr_sum_idx(:) = 0.d0
qr_div_idx(:) = 0.d0

! This function (QR_CALC) is parallelised
call qr_calc(hr_idx, qr_idx)
if( div_switch .eq. 1 ) call RRI_Div(qr_idx, qr_div_idx)

! boundary condition for river (water depth boundary)
if( bound_riv_wlev_switch .ge. 1 ) then
 itemp = -1
 do jtemp = 1, tt_max_bound_riv_wlev
  if( t_bound_riv_wlev(jtemp-1) .lt. (time + ddt) .and. (time + ddt) .le. t_bound_riv_wlev(jtemp) ) itemp = jtemp
 enddo
 do k = 1, riv_count
  if( bound_riv_wlev_idx(itemp, k) .le. -100.0 ) cycle ! not boundary
  hr_idx(k) = bound_riv_wlev_idx(itemp, k)
 enddo
endif

! boundary condition for river (discharge boundary)
if( bound_riv_disc_switch .ge. 1 ) then
 do jtemp = 1, tt_max_bound_riv_disc
  if( t_bound_riv_disc(jtemp-1) .lt. (time + ddt) .and. (time + ddt) .le. t_bound_riv_disc(jtemp) ) itemp = jtemp
 enddo
 do k = 1, riv_count
  if( bound_riv_disc_idx(itemp, k) .le. -100.0 ) cycle ! not boundary
  qr_idx(k) = bound_riv_disc_idx(itemp, k) / area  ! qr_idx: discharge per unit area
  ! linear interpolation of the boundary condition
  !qr_idx(k) = bound_riv_disc_idx(itemp-1, k) * (t_bound_riv_disc(itemp) - (time + ddt)) &
  !            + bound_riv_disc_idx(itemp, k) * ((time + ddt) - t_bound_riv_disc(itemp-1))
  !qr_idx(k) = qr_idx(k) / (t_bound_riv_disc(itemp) - t_bound_riv_disc(itemp-1)) / area 
  hr_idx(k) = 0.d0
 enddo
endif

! dam control
if( dam_switch .eq. 1 ) then
 call dam_prepare(qr_idx) ! calculate inflow to dam
 do i = 1, dam_num
  if( dam_volmax(i) .ge. 0.d0 ) then
   ! dam
   call dam_operation( dam_loc(i) )
   qr_idx( dam_loc(i) ) = dam_qout(i)
   hr_idx( dam_loc(i) ) = 0.d0
  else
   ! barrage
   if( hr_idx( dam_loc(i) ) .le. dam_floodq(i) ) then
    qr_idx( dam_loc(i) ) = 0.d0
   else
    continue
   endif
  endif
 enddo
endif

! qr_sum > 0 --> discharge flowing out from a cell
do k = 1, riv_count
 ! outflow from (k)
 qr_sum_idx(k) = qr_sum_idx(k) + qr_idx(k)
 kk = down_riv_idx(k)
 if(domain_riv_idx(kk).eq.0) cycle
 ! qr_sum minus (flowing into) discharge at the destination cell
 qr_sum_idx(kk) = qr_sum_idx(kk) - qr_idx(k)  
enddo

! diversion
if( div_switch .eq. 1 ) then
 do l = 1, div_id_max
 ! outflow from (k)
  k = div_org_idx(l)
  qr_sum_idx(k) = qr_sum_idx(k) + qr_div_idx(k)
  kk = div_dest_idx(l)
  if(domain_riv_idx(kk).eq.0) cycle
  ! qr_sum minus (flowing into) discharge at the destination cell
  qr_sum_idx(kk) = qr_sum_idx(kk) - qr_div_idx(k) 
 enddo
endif

! qr_sum divide by area_ratio
! (area_ratio = ratio of river area against total cell area) 
fr_idx = -qr_sum_idx / area_ratio_idx

end subroutine funcr


! lateral discharge (river)

subroutine qr_calc(hr_idx, qr_idx)
use globals
use dam_mod, only: damflg
implicit none

real(8) hr_idx(riv_count), qr_idx(riv_count), qr_div_idx(riv_count)

integer k, kk
real(8) zb_p, hr_p
real(8) zb_n, hr_n
real(8) dh, distance
real(8) qr_temp, hw
integer dif_p, dif_n

qr_idx(:) = 0.d0
qr_div_idx(:) = 0.d0

 !$omp parallel do private(kk,zb_p,hr_p,distance,zb_n,hr_n,dh,hw,qr_temp)
do k = 1, riv_count

 if(domain_riv_idx(k) .eq. 2) cycle

 zb_p = zb_riv_idx(k)
 hr_p = hr_idx(k)
 dif_p = dif_riv_idx(k)

 distance = dis_riv_idx(k)

 ! information of the destination cell
 kk = down_riv_idx(k)
 zb_n = zb_riv_idx(kk)
 hr_n = hr_idx(kk)
 dif_n = dif_riv_idx(kk)

 ! diffusion wave
 dh = ((zb_p + hr_p) - (zb_n + hr_n)) / distance ! diffussion
 !dh = max( (zb_p - zb_n) / distance, 0.001 ) ! kinematic

 ! kinematic wave
 if( dif_p .eq. 0 ) dh = max( (zb_p - zb_n) / distance, 0.001 )

 ! the destination cell is outlet (domain = 2)
 if( domain_riv_idx(kk) .eq. 2 ) dh = (zb_p + hr_p - zb_n) / distance ! kinematic wave (+hr_p)

 ! the destination cell is outlet (domain = 2 ) with water depth boundary water table
 if( domain_riv_idx(kk) .eq. 2 .and. bound_riv_wlev_switch .ge. 1 ) then
  if( bound_riv_wlev_idx(1, kk) .le. -100.0 ) cycle ! not boundary
  dh = ((zb_p + hr_p) - (zb_n + hr_n)) / distance ! diffussion
 endif

 ! the cell or the destination cell is dam (damflg(k) or damflg(kk) > 0)
 if ( damflg(k) .gt. 0 .or. damflg(kk) .gt. 0 ) dh = (zb_p - zb_n) / distance ! kinematic wave

 ! from a tributary (levee height =< 0) to a main river (levee height > 0) : no reverse flow
 if( height_idx(k) .le. 0.d0 .and. height_idx(kk) .gt. 0.d0 ) dh = max( (zb_p - zb_n) / distance, 0.001 )

 if( dh .ge. 0.d0 ) then
  hw = hr_p
  if( zb_p .lt. zb_n ) hw = max(0.d0, zb_p + hr_p - zb_n)
  call hq_riv(hw, dh, width_idx(k), qr_temp)
  qr_idx(k) = qr_temp
 else
  ! reverse flow
  hw = hr_n
  if( zb_n .lt. zb_p ) hw = max(0.d0, zb_n + hr_n - zb_p)
  dh = abs(dh)
  call hq_riv(hw, dh, width_idx(k), qr_temp)
  qr_idx(k) = -qr_temp
 endif

enddo
 !$omp end parallel do

end subroutine qr_calc

! water depth and discharge relationship
subroutine hq_riv(h, dh, w, q)
use globals
implicit none

real(8) h, dh, w, q
real(8) a, m

a = sqrt(abs(dh)) / ns_river
m = 5.d0 / 3.d0

q = a * h ** m

! discharge per unit area
! (q multiply by width and divide by area)
q = q * w / area

end subroutine hq_riv
