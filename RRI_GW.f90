! RRI_GW

! current version of RRI model does not call this subroutine
! this subroutine is still under development

! ground water
subroutine gw( hs_idx )
use globals
implicit none

real(8) hs_idx(slo_count)

real(8), parameter :: kgh = 0.01d0 ! [mm/h] -> [m/s]
real(8), parameter :: kgv = 0.2d0 / 1000.d0 / 3600.d0 ! [mm/h] -> [m/s]

real(8) zb_p, hs_p, zb_n, hs_n
real(8) dh, distance, len
real(8) qgh(slo_count), qgv(slo_count)
integer k, kk, i, j

! horizontal groundwater discharge
do k = 1, slo_count

 kk = down_slo_1d_idx(k)
 if( kk .eq. -1 ) cycle

 zb_p = zb_slo_idx(k)
 hs_p = hs_idx(k)
 distance = dis_slo_1d_idx(k)
 len = len_slo_1d_idx(k)
 zb_n = zb_slo_idx(kk)
 hs_n = hs_idx(kk)
 dh = max( (zb_p - zb_n) / distance, 0.001d0 )

 ! discharge per unit area
 qgh(k) = kgh * dh * hg_idx(k) * len / area

enddo

! vertical groundwater movement
do k = 1, slo_count
 i = slo_idx2i(k)
 j = slo_idx2j(k)
 qgv(k) = min( qgv(k), (hs_idx(k) + gampt_ff(i, j)) / dt )
 gampt_ff(i, j) = gampt_ff(i, j) - qgv(k) * dt
 if( gampt_ff(i, j) .lt. 0.d0 ) then
  hs_idx(k) = hs_idx(k) + gampt_ff(i, j)
  gampt_ff(i, j) = 0.d0
  if( hs_idx(k) .lt. 0.d0 ) then
   hs_idx(k) = 0.d0
  endif
 endif
 hg_idx(k) = hg_idx(k) + qgv(k) * dt

enddo

! horizontal groundwater movement
do k = 1, slo_count

 kk = down_slo_1d_idx(k)
 if( kk .eq. -1 ) cycle

 qgh(k) = min( hg_idx(k) / dt, qgh(k) )
 hg_idx(k) = hg_idx(k) - qgh(k) * dt
 if(hg_idx(k).lt.0.d0) hg_idx(k) = 0.d0
 if(riv(slo_idx2i(k), slo_idx2j(k)) .eq. 0) then
  ! slope cell
  hg_idx(kk) = hg_idx(kk) + qgh(k) * dt
 else
  ! river cell : return to hs
  hs_idx(k) = hs_idx(k) + qgh(k) * dt
 endif

enddo

end subroutine gw
