! Output for T-SAS program by RRI
!
subroutine RRI_TSAS(tt, hs_idx, hr_idx, qs_ave_idx, qr_ave_idx, qp_t_idx)
use globals
use dam_mod
implicit none

integer tt

character*256 tsasfolder, tsasdata
character*256 ofile_tsas_hs_id, ofile_tsas_hr_id, ofile_tsas_outlet
character*256 ofile_tsas_sto, ofile_tsas_flux, ofile_tsas_rain

real(8) hs_idx(slo_count), hr_idx(riv_count), qs_ave_idx(i4, slo_count)
real(8) qr_ave_idx(riv_count), qp_t_idx(slo_count)

integer tsasid, dif_p
integer i, j, k, l, kk
character*6 t_char

parameter( tsasfolder = "./tsas/" )
parameter( tsasdata = "./tsas/data/" )

call int2char(tt, t_char)

! Initial Output
if(tt.eq.0) then

 ofile_tsas_hs_id = trim(tsasfolder) // "hs_id.txt"
 ofile_tsas_hr_id = trim(tsasfolder) // "hr_id.txt"
 ofile_tsas_outlet = trim(tsasfolder) // "outlet.txt"

 open( 1001, file = ofile_tsas_hs_id )
 open( 1002, file = ofile_tsas_hr_id )
 open( 1003, file = ofile_tsas_outlet )

 allocate( hs_id(ny, nx), hr_id(ny, nx) )
 allocate( hs_id_idx(slo_count), hr_id_idx(riv_count) )

 hs_id(:,:) = -999
 hr_id(:,:) = -999

 tsasid = 0

 do k = 1, slo_count
  tsasid = tsasid + 1
  i = slo_idx2i(k)
  j = slo_idx2j(k)
  hs_id(i, j) = tsasid
  hs_id_idx(k) = tsasid
  if( domain_slo_idx(k) .eq. 2 ) write(1003, *) tsasid
 enddo

 do k = 1, riv_count
  tsasid = tsasid + 1
  i = riv_idx2i(k)
  j = riv_idx2j(k)
  hr_id(i, j) = tsasid
  hr_id_idx(k) = tsasid
  if( domain_riv_idx(k) .eq. 2 ) write(1003, *) tsasid
 enddo

 write(1001, '("ncols", i15)') nx
 write(1001, '("nrows", i15)') ny
 write(1001, '("xllcorner", f20.12)') xllcorner
 write(1001, '("yllcorner", f20.12)') yllcorner
 write(1001, '("cellsize", f20.12)') cellsize
 write(1001, '("NODATA_value", i10)') -999

 write(1002, '("ncols", i15)') nx
 write(1002, '("nrows", i15)') ny
 write(1002, '("xllcorner", f20.12)') xllcorner
 write(1002, '("yllcorner", f20.12)') yllcorner
 write(1002, '("cellsize", f20.12)') cellsize
 write(1002, '("NODATA_value", i10)') -999

 do i = 1, ny
  write(1001, 10001) (hs_id(i, j), j = 1, nx)
  write(1002, 10002) (hr_id(i, j), j = 1, nx)
 enddo

 close(1001)
 close(1002)
 close(1003)

endif


! Data Output

call int2char(tt, t_char)

ofile_tsas_sto = trim(tsasdata) // "tsas_sto_" // trim(t_char) // ".txt"
ofile_tsas_flux = trim(tsasdata) // "tsas_flux_" // trim(t_char) // ".txt"
ofile_tsas_rain = trim(tsasdata) // "tsas_rain_" // trim(t_char) // ".txt"

open(1003, file = ofile_tsas_sto)
open(1004, file = ofile_tsas_flux)
open(1005, file = ofile_tsas_rain)

! storage

do k = 1, slo_count
 write(1003, 10003) hs_idx(k) * area, 1
enddo
do k = 1, riv_count
 i = riv_idx2i(k)
 j = riv_idx2j(k)
 write(1003, 10003) hr_idx(k) * area * area_ratio(i, j), 2
enddo

! flux [m3/s]

! qs
do k = 1, slo_count
 do l = 1, lmax
  dif_p = dif_slo_idx(k)
  if( dif_p .eq. 0 .and. l .eq. 2 ) exit ! kinematic -> 1-direction
  if( dif_p .eq. 0 ) kk = down_slo_1d_idx(k)
  kk = down_slo_idx(l, k)
  if( kk .eq. -1 ) cycle
  write(1004, 10004) hs_id_idx(k), hs_id_idx(kk), (qs_ave_idx(l, k) * area), 1
 enddo
enddo

! qr
do k = 1, riv_count
 if( domain_riv_idx(k) .eq. 2) cycle
 kk = down_riv_idx(k)
 write(1004, 10004) hr_id_idx(k), hr_id_idx(kk), (qr_ave_idx(k) * area), 2
enddo

! qrs
do i = 1, ny
 do j = 1, nx
  if( domain(i,j) .eq. 0 ) cycle
  if( riv(i, j) .eq. 0 ) cycle
  write(1004, 10004) hs_id(i, j), hr_id(i, j), (qrs(i, j) * area), 3
 enddo
enddo


! id, rainfall, aevp, infilt

do k = 1, slo_count
 i = slo_idx2i(k)
 j = slo_idx2j(k)
 write(1005, 10005) hs_id_idx(k), (qp_t_idx(k) * area), (aevp_tsas(k) * area), (gampt_f(i, j) * area)
enddo


10001 format(100000i7) ! hs_id
10002 format(100000i7) ! hr_id
10003 format(e18.10, i3) ! sto
10004 format(2i7, e18.10, i3) ! flux
10005 format(i7, 3e18.10) ! rain

end subroutine RRI_TSAS
