! RRI.f90
!
! coded by T.Sayama
!
! ver 1.3.3
!
program RRI
use globals
use runge_mod
use dam_mod, only: dam_switch, dam_vol_temp
use tecout_mod
use omp_lib
implicit none

! variable definition

! topographic variable
integer nodata
real(8) x1, x2, y1, y2, d1, d2, d3, d4

! rainfall variable
integer, allocatable :: rain_i(:), rain_j(:)
integer tt_max_rain
integer, allocatable :: t_rain(:)
integer nx_rain, ny_rain
real(8), allocatable :: qp(:,:,:), qp_t(:,:), qp_t_idx(:)

! calculation variable
real(8) rho, total_area

real(8), allocatable :: hs(:,:), hr(:,:), inith(:,:)
real(8), allocatable :: qs_ave(:,:,:), qr_ave(:,:)

real(8), allocatable :: fs(:), hs_idx(:), fr(:), hr_idx(:)
real(8), allocatable :: qr_idx(:), qr_ave_idx(:), qr_ave_temp_idx(:)
real(8), allocatable :: qs_idx(:,:), qs_ave_idx(:,:), qs_ave_temp_idx(:,:)

!real(8), allocatable :: rdummy_dim(:)

! other variable
integer i, j, t, k, ios, itemp, jtemp, tt, ii, jj
integer out_next
real(8) out_dt
real(8) rtemp
real(8) ss, sr, si, sg, sinit, sout
integer num_of_cell
integer idummy
real(8) rdummy
real(8) rain_sum
real(8) distance
real(8) ddt_chk_riv, ddt_chk_slo
character*256 ctemp
character*6 t_char
integer div_org_i, div_org_j, div_dest_i, div_dest_j

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! STEP 0: FILE NAME AND PARAMETER SETTING
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
call RRI_Read

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! STEP 1: FILE READING 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! max timestep
maxt = lasth * 3600 / dt

! dem file
open( 10, file = demfile, status = "old" )
read(10,*) ctemp, nx
read(10,*) ctemp, ny
read(10,*) ctemp, xllcorner
read(10,*) ctemp, yllcorner
read(10,*) ctemp, cellsize
read(10,*) ctemp, nodata
close(10)

allocate (zs(ny, nx), zb(ny, nx), zb_riv(ny, nx), domain(ny, nx))
call read_gis_real(demfile, zs)

! flow accumulation file
allocate (riv(ny, nx), acc(ny, nx))
call read_gis_int(accfile, acc)

! flow direction file
allocate (dir(ny, nx))
call read_gis_int(dirfile, dir)

! landuse file
allocate( land(ny, nx) )
land = 1
if( land_switch.eq.1 ) then
 call read_gis_int(landfile, land)
endif

! land : 1 ... num_of_landuse
write(*,*) "num_of_landuse : ", num_of_landuse
where( land .le. 0 .or. land .gt. num_of_landuse ) land = num_of_landuse

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! STEP 2: CALC PREPARATION 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! dx, dy calc
! d1: south side length
x1 = xllcorner
y1 = yllcorner
x2 = xllcorner + nx * cellsize
y2 = yllcorner
if( utm.eq.0 ) call hubeny_sub( x1, y1, x2, y2, d1 )

! d2: north side length
x1 = xllcorner
y1 = yllcorner + ny * cellsize
x2 = xllcorner + nx * cellsize
y2 = yllcorner + ny * cellsize
if( utm.eq.0 ) call hubeny_sub( x1, y1, x2, y2, d2 )

! d3: west side length
x1 = xllcorner
y1 = yllcorner
x2 = xllcorner
y2 = yllcorner + ny * cellsize
if( utm.eq.0 ) call hubeny_sub( x1, y1, x2, y2, d3 )

! d4: east side length
x1 = xllcorner + nx * cellsize
y1 = yllcorner
x2 = xllcorner + nx * cellsize
y2 = yllcorner + ny * cellsize
if( utm.eq.0 ) call hubeny_sub( x1, y1, x2, y2, d4 )

if( utm.eq.1 ) then
 dx = cellsize
 dy = cellsize
else
 dx = (d1 + d2) / 2.d0 / real(nx)
 dy = (d3 + d4) / 2.d0 / real(ny)
endif
write(*,*) "dx [m] : ", dx, "dy [m] : ", dy

! length and area of each cell
length = sqrt(dx * dy)
area = dx * dy

! river widhth, depth, leavy height, river area ratio
allocate ( width(ny, nx), depth(ny, nx), height(ny, nx), area_ratio(ny, nx) )

width = 0.d0
depth = 0.d0
height = 0.d0

area_ratio = 0.d0

riv = 0 ! slope cell
if( riv_thresh .gt. 0 ) then
 where(acc .gt. riv_thresh) riv = 1 ! river cell
endif

where(riv.eq.1) width = width_param_c * ( acc * dx * dy * 1.d-6 ) ** width_param_s
where(riv.eq.1) depth = depth_param_c * ( acc * dx * dy * 1.d-6 ) ** depth_param_s
where(riv.eq.1 .and. acc.gt.height_limit_param) height = height_param

! river data is replaced by the information in files
if( rivfile_switch .ge. 1 ) then
 riv = 0
 riv_thresh = 1
 call read_gis_real(widthfile, width)
 where(width .gt. 0) riv = 1 ! river cells (if width >= 0.)
 call read_gis_real(depthfile, depth)
 call read_gis_real(heightfile, height)
 where( height(:,:) .lt. 0.d0 ) height(:,:) = 0.d0 ! added (v.1.3.1)
endif 

if( rivfile_switch .eq. 2 ) then
 ! levee on both river and slope grid cells : zs is increased with height
 where( height .gt. 0.d0 ) zs = zs + height
else
 ! levee on only slope grid cells : zs is increased with height
 where( height .gt. 0.d0 .and. riv .eq. 0 ) zs = zs + height
endif


where(riv.eq.1) area_ratio = width / length

zb_riv = zs
do i = 1, ny
 do j = 1, nx
  zb(i, j) = zs(i, j) - soildepth(land(i,j))
  if(riv(i, j) .eq. 1) zb_riv(i, j) = zs(i, j) - depth(i, j)
 enddo
enddo

! domain setting

! domain = 0 : outside the domain
! domain = 1 : inside the domain
! domain = 2 : outlet point (where dir(i,j) = 0 or dir(i,j) = -1),
!              and cells located at edges
domain = 0
num_of_cell = 0
do i = 1, ny
 do j = 1, nx
  if( zs(i, j) .gt. -100.d0 ) then
   domain(i, j) = 1
   if( dir(i, j).eq.0 ) domain(i, j) = 2
   if( dir(i, j).eq.-1 ) domain(i, j) = 2
   num_of_cell = num_of_cell + 1
  endif
 enddo
enddo
write(*,*) "num_of_cell : ", num_of_cell
write(*,*) "total area [km2] : ", num_of_cell * area / (10.d0 ** 6.0d0)

! river index setting
call riv_idx_setting

! slope index setting
call slo_idx_setting

! reading dam file
call dam_read

! initial condition

allocate(hs(ny, nx), hr(ny, nx), gampt_ff(ny, nx))
allocate(gampt_f(ny, nx), qrs(ny, nx))

hr = -0.1d0
hs = -0.1d0
gampt_ff = 0.d0
gampt_f = 0.d0
qrs = 0.d0

where(riv.eq.1) hr = init_cond_riv
where(domain.eq.1) hs = init_cond_slo
where(domain.eq.2) hs = 0.d0

! for slope cells
! if init_slo_switch = 1 => read from file

if(init_slo_switch .eq. 1) then

 allocate( inith(ny, nx) )
 inith = 0.d0
 open(13, file = initfile_slo, status = "old")
 do i = 1, ny
  read(13,*) (inith(i,j), j = 1, nx)
 enddo
 where(inith .le. 0.d0) inith = 0.d0
 where(domain.eq.1 .and. inith .ge. 0.d0) hs = inith
 deallocate( inith )
 close(13)

endif

! for river cells
! if init_riv_switch = 1 => read from file

if(init_riv_switch .eq. 1) then

 allocate( inith(ny, nx) )
 inith = 0.d0
 open(13, file = initfile_riv, status = "old")
 do i = 1, ny
  read(13,*) (inith(i,j), j = 1, nx)
 enddo
 where(inith .le. 0.d0) inith = 0.d0
 where(riv.eq.1 .and. inith .ge. 0.d0) hr = inith
 deallocate( inith )
 close(13)

endif

! if init_gampt_ff_switch = 1 => read from file

if(init_gampt_ff_switch .eq. 1) then

 allocate( inith(ny, nx) )
 inith = 0.d0
 open(13, file = initfile_gampt_ff, status = "old")
 do i = 1, ny
  read(13,*) (inith(i,j), j = 1, nx)
 enddo
 where(inith .le. 0.d0) inith = 0.d0
 where(domain.eq.1) gampt_ff = inith
 deallocate( inith )
 close(13)

endif

! boundary conditions
call read_bound

! div file
div_id_max = 0
if( div_switch.eq.1 ) then
 open( 20, file = divfile, status = "old" )
 do
  read(20, *, iostat = ios) div_org_i, div_org_j, div_dest_i, div_dest_j
  if(ios .ne. 0) exit
  div_id_max = div_id_max + 1
 enddo
 write(*,*) "div_id_max : ", div_id_max
 allocate( div_org_idx(div_id_max), div_dest_idx(div_id_max) )
 rewind(20)

 do k = 1, div_id_max
  read(20, *) div_org_i, div_org_j, div_dest_i, div_dest_j
  div_org_idx(k) = riv_ij2idx( div_org_i, div_org_j )
  div_dest_idx(k) = riv_ij2idx( div_dest_i, div_dest_j )
 enddo
 write(*,*) "done: reading div file"
 close(20)
endif

! emb file
!if( emb_switch.eq.1 ) then
! allocate (emb_r(ny, nx), emb_b(ny, nx))
! call read_gis_real(embrfile, emb_r)
! call read_gis_real(embbfile, emb_b)

! call sub_slo_ij2idx(emb_r, emb_r_idx)
! call sub_slo_ij2idx(emb_b, emb_b_idx)
!endif


! dynamic allocation
allocate (qs_ave(i4, ny, nx), qr_ave(ny, nx))

allocate (qr_idx(riv_count), qr_ave_idx(riv_count), qr_ave_temp_idx(riv_count), hr_idx(riv_count))
allocate (fr(riv_count), hr_temp(riv_count), hr_err(riv_count)) 
allocate (kr2(riv_count), kr3(riv_count), kr4(riv_count), kr5(riv_count), kr6(riv_count))

allocate (qs_idx(i4, slo_count), qs_ave_idx(i4, slo_count), qs_ave_temp_idx(i4, slo_count), hs_idx(slo_count))
allocate (qp_t_idx(slo_count))
allocate (fs(slo_count), hs_temp(slo_count), hs_err(slo_count))
allocate (ks2(slo_count), ks3(slo_count), ks4(slo_count), ks5(slo_count), ks6(slo_count))

allocate (rain_i(ny), rain_j(nx))
allocate (ksv(ny, nx), delta(ny, nx), faif(ny, nx), infilt_limit(ny, nx))
allocate (qe_t_idx(slo_count))
allocate (evp_i(ny), evp_j(nx))
allocate (aevp(ny, nx), aevp_tsas(slo_count))
allocate (hg(ny, nx), hg_idx(slo_count))

! array initialization
qr_ave(:,:) = 0.d0
qr_idx(:) = 0.d0
qr_ave_idx(:) = 0.d0
qr_ave_temp_idx(:) = 0.d0
hr_idx(:) = 0.d0
fr(:) = 0.d0
hr_temp(:) = 0.d0
hr_err(:) = 0.d0
kr2(:) = 0.d0
kr3(:) = 0.d0
kr4(:) = 0.d0
kr5(:) = 0.d0
kr6(:) = 0.d0

qs_ave(:,:,:) = 0.d0
qs_idx(:,:) = 0.d0
qs_ave_idx(:,:) = 0.d0
qs_ave_temp_idx(:,:) = 0.d0
hs_idx(:) = 0.d0
qp_t_idx(:) = 0.d0
fs(:) = 0.d0
hs_temp(:) = 0.d0
hs_err(:) = 0.d0
ks2(:) = 0.d0
ks3(:) = 0.d0
ks4(:) = 0.d0
ks5(:) = 0.d0
ks6(:) = 0.d0

rain_i(:) = 0
rain_j(:) = 0
ksv(:,:) = 0.d0
delta(:,:) = 0.d0
faif(:,:) = 0.d0
infilt_limit(:,:) = 0.d0
aevp(:,:) = 0.d0
hg(:,:) = 0.d0
hg_idx(:) = 0.d0
evp_i(:) = 0
evp_j(:) = 0
aevp_tsas(:) = 0.d0

! infiltration parameter 1d -> 2d
do i = 1, ny
 do j = 1, nx
  if( domain(i, j) .eq. 0 ) cycle
  ksv(i, j) = ksv_1d( land(i, j) )
  delta(i, j) = delta_1d( land(i, j) )
  faif(i, j) = faif_1d( land(i, j) )
  infilt_limit(i, j) = infilt_limit_1d( land(i, j) )
 enddo
enddo

! initial storage calculation
rain_sum = 0.d0
aevp_sum = 0.d0
pevp_sum = 0.d0
sout = 0.d0
si = 0.d0
sg = 0.d0
open( 1000, file = outfile_storage )
call storage_calc(hs, hr, ss, sr, si, sg)
sinit = ss + sr + si + sg
write(1000, '(1000e15.7)') rain_sum, pevp_sum, aevp_sum, sout, ss + sr + si + sg, (rain_sum - aevp_sum - sout - sinit - (ss + sr &
        + si + sg)), ss, sr, si, sg

! reading rainfall data
open( 11, file = rainfile, status = 'old' )

tt = 0
do
 read(11, *, iostat = ios) t, nx_rain, ny_rain
 do i = 1, ny_rain
  read(11, *, iostat = ios) rdummy
 enddo
 if( ios /= 0 ) exit
 tt = tt + 1
enddo
tt_max_rain = tt - 1

allocate( t_rain(0:tt_max_rain), qp(0:tt_max_rain, ny_rain, nx_rain), qp_t(ny, nx) )
rewind(11)

qp = 0.d0
do tt = 0, tt_max_rain
 read(11, *) t_rain(tt), nx_rain, ny_rain
 do i = 1, ny_rain
  read(11, *) (qp(tt, i, j), j = 1, nx_rain)
 enddo
enddo
! unit convert from (mm/h) to (m/s)
qp = qp / 3600.d0 / 1000.d0

do j = 1, nx
 rain_j(j) = int( (xllcorner + (dble(j) - 0.5d0) * cellsize - xllcorner_rain) / cellsize_rain_x ) + 1
enddo
do i = 1, ny
 rain_i(i) = ny_rain - int( (yllcorner + (dble(ny) - dble(i) + 0.5d0) * cellsize - yllcorner_rain) / cellsize_rain_y )
enddo
close(11)

write(*,*) "done: reading rain file"

! reading evp data
if( evp_switch .ne. 0 ) then
 open( 11, file = evpfile, status = 'old' )

 tt = 0
 do
  read(11, *, iostat = ios) t, nx_evp, ny_evp
  do i = 1, ny_evp
   read(11, *, iostat = ios) rdummy
  enddo
  if( ios /= 0 ) exit
  tt = tt + 1
 enddo
 tt_max_evp = tt - 1

 allocate( t_evp(0:tt_max_evp), qe(0:tt_max_evp, ny_evp, nx_evp), qe_t(ny, nx) )
 rewind(11)

 write(*,*) "done: reading evp file"

 qe = 0.d0
 do tt = 0, tt_max_evp
  read(11, *) t_evp(tt), nx_evp, ny_evp
  do i = 1, ny_evp
   read(11, *) (qe(tt, i, j), j = 1, nx_evp)
  enddo
 enddo
 ! unit convert from (mm/h) to (m/s)
 qe = qe / 3600.d0 / 1000.d0

 do j = 1, nx
  evp_j(j) = int( (xllcorner + (dble(j) - 0.5d0) * cellsize - xllcorner_evp) / cellsize_evp_x ) + 1
 enddo
 do i = 1, ny
  evp_i(i) = ny_evp - int( (yllcorner + (dble(ny) - dble(i) + 0.5d0) * cellsize - yllcorner_evp) / cellsize_evp_y )
 enddo
 close(11)
endif

! For TSAS Output (Initial Condition)
!call RRI_TSAS(0, hs_idx, hr_idx, qs_ave_idx, qr_ave_idx, qp_t_idx)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! STEP 3: CALCULATION 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!write(*,*) "Step3: calculation"

rain_sum = 0.d0
aevp_sum = 0.d0
sout = 0.d0

! output timestep
out_dt = dble(maxt) / dble(outnum)
out_dt = max(1.d0, out_dt)
out_next = nint(out_dt)
!****YANN****
out_dt = 1
tt = 0

do t = 1, maxt

 if(mod(t, 1).eq.0) write(*,*) t, "/", maxt

 !******* RIVER CALCULATION ******************************
 if( riv_thresh .lt. 0 ) go to 2

 ! from time = (t - 1) * dt to t * dt
 time = (t - 1) * dt ! (current time)
 ! time step is initially set to be "dt_riv"
 ddt = dt_riv
 ddt_chk_riv = dt_riv

 qr_ave = 0.d0
 qr_ave_idx = 0.d0
 if( dam_switch .eq. 1 ) dam_vol_temp(:) = 0.d0

 ! hr -> hr_idx
 ! Memo: riv_ij2idx must be here. 
 ! hr_idx cannot be replaced within the following do loop.
 call sub_riv_ij2idx( hr, hr_idx )

 do

  ! "time + ddt" should be less than "t * dt"
  if(time + ddt .gt. t * dt ) ddt = t * dt - time

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

1 continue
  qr_ave_temp_idx(:) = 0.d0

  ! Adaptive Runge-Kutta 
  ! (1)
  call funcr( hr_idx, fr, qr_idx )
  hr_temp = hr_idx + b21 * ddt * fr
  where(hr_temp .lt. 0) hr_temp = 0.d0
  qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

  ! (2)
  call funcr( hr_temp, kr2, qr_idx )
  hr_temp = hr_idx + ddt * (b31 * fr + b32 * kr2)
  where(hr_temp .lt. 0) hr_temp = 0.d0
  qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

  ! (3)
  call funcr( hr_temp, kr3, qr_idx )
  hr_temp = hr_idx + ddt * (b41 * fr + b42 * kr2 + b43 * kr3)
  where(hr_temp .lt. 0) hr_temp = 0.d0
  qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

  ! (4)
  call funcr( hr_temp, kr4, qr_idx )
  hr_temp = hr_idx + ddt * (b51 * fr + b52 * kr2 + b53 * kr3 + b54 * kr4)
  where(hr_temp .lt. 0) hr_temp = 0.d0
  qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

  ! (5)
  call funcr( hr_temp, kr5, qr_idx )
  hr_temp = hr_idx + ddt * (b61 * fr + b62 * kr2 + b63 * kr3 + b64 * kr4 + b65 * kr5)
  where(hr_temp .lt. 0) hr_temp = 0.d0
  qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

  ! (6)
  call funcr( hr_temp, kr6, qr_idx )
  hr_temp = hr_idx + ddt * (c1 * fr + c3 * kr3 + c4 * kr4 + c6 * kr6)
  where(hr_temp .lt. 0) hr_temp = 0.d0
  qr_ave_temp_idx = qr_ave_temp_idx + qr_idx * ddt

  ! (e)
  hr_err = ddt * (dc1 * fr + dc3 * kr3 + dc4 * kr4 + dc5 * kr5 + dc6 * kr6)

  ! error evaluation
  where( domain_riv_idx .eq. 0 ) hr_err = 0
  errmax = maxval( hr_err ) / eps

  if(errmax.gt.1 .and. ddt .ge. ddt_min_riv) then
   ! try smaller ddt
   ddt = max( safety * ddt * (errmax ** pshrnk), 0.5d0 * ddt )
   ddt_chk_riv = ddt
   !write(*,*) "shrink (riv): ", ddt, errmax, maxloc( hr_err )
   if(ddt.eq.0) stop 'stepsize underflow'
   if(dam_switch .eq. 1 ) dam_vol_temp(:) = 0.d0
   go to 1
  else
   ! "time + ddt" should be less than "t * dt"
   if(time + ddt .gt. t * dt ) ddt = t * dt - time
   time = time + ddt
   hr_idx = hr_temp
   qr_ave_idx = qr_ave_idx + qr_ave_temp_idx
  endif
  if(time.ge.t * dt) exit ! finish for this timestep
 enddo
 qr_ave_idx = qr_ave_idx / dble(dt) / 6.d0

 ! hr_idx -> hr, qr_ave_idx -> qr_ave
 call sub_riv_idx2ij( hr_idx, hr )
 call sub_riv_idx2ij( qr_ave_idx, qr_ave )

 if( dam_switch.eq.1 ) call dam_checkstate(qr_ave)

 !******* SLOPE CALCULATION ******************************
2 continue

 ! from time = (t - 1) * dt to t * dt
 time = (t - 1) * dt  ! (current time)
 ! time step is initially set to be "dt"
 ddt = dt
 ddt_chk_slo = dt

 qs_ave = 0.d0
 qs_ave_idx = 0.d0

 ! hs -> hs_idx
 ! Memo: slo_ij2idx must be here. 
 ! hs_idx cannot be replaced within the following do loop.
 call sub_slo_ij2idx( hs, hs_idx )

 do

  if(time + ddt .gt. t * dt ) ddt = t * dt - time

  ! rainfall
  itemp = -1
  do jtemp = 1, tt_max_rain
   if( t_rain(jtemp-1) .lt. (time + ddt) .and. (time + ddt) .le. t_rain(jtemp) ) itemp = jtemp
  enddo
  do i = 1, ny
   do j = 1, nx
    qp_t(i, j) = qp(itemp, rain_i(i), rain_j(j))
   enddo
  enddo
  call sub_slo_ij2idx( qp_t, qp_t_idx )

  ! boundary condition for slope (water depth boundary)
  if( bound_slo_wlev_switch .ge. 1 ) then
   itemp = -1
   do jtemp = 1, tt_max_bound_slo_wlev
    if( t_bound_slo_wlev(jtemp-1) .lt. (time + ddt) .and. (time + ddt) .le. t_bound_slo_wlev(jtemp) ) itemp = jtemp
   enddo
   do k = 1, slo_count
    if( bound_slo_wlev_idx(itemp, k) .le. -100.0 ) cycle ! not boundary
    hs_idx(k) = bound_slo_wlev_idx(itemp, k)
   enddo
  endif

3 continue
  qs_ave_temp_idx(:,:) = 0.d0

  ! Adaptive Runge-Kutta 
  ! (1)
  call funcs( hs_idx, qp_t_idx, fs, qs_idx )
  hs_temp = hs_idx + b21 * ddt * fs
  where(hs_temp .lt. 0) hs_temp = 0.d0
  qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt

  ! (2)
  call funcs( hs_temp, qp_t_idx, ks2, qs_idx )
  hs_temp = hs_idx + ddt * (b31 * fs + b32 * ks2)
  where(hs_temp .lt. 0) hs_temp = 0.d0
  qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt

  ! (3)
  call funcs( hs_temp, qp_t_idx, ks3, qs_idx )
  hs_temp = hs_idx + ddt * (b41 * fs + b42 * ks2 + b43 * ks3)
  where(hs_temp .lt. 0) hs_temp = 0.d0
  qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt

  ! (4)
  call funcs( hs_temp, qp_t_idx, ks4, qs_idx )
  hs_temp = hs_idx + ddt * (b51 * fs + b52 * ks2 + b53 * ks3 + b54 * ks4)
  where(hs_temp .lt. 0) hs_temp = 0.d0
  qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt

  ! (5)
  call funcs( hs_temp, qp_t_idx, ks5, qs_idx )
  hs_temp = hs_idx + ddt * (b61 * fs + b62 * ks2 + b63 * ks3 + b64 * ks4 + b65 * ks5)
  where(hs_temp .lt. 0) hs_temp = 0.d0
  qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt

  ! (6)
  call funcs( hs_temp, qp_t_idx, ks6, qs_idx )
  hs_temp = hs_idx + ddt * (c1 * fs + c3 * ks3 + c4 * ks4 + c6 * ks6)
  where(hs_temp .lt. 0) hs_temp = 0.d0
  qs_ave_temp_idx = qs_ave_temp_idx + qs_idx * ddt

  ! (e)
  hs_err = ddt * (dc1 * fs + dc3 * ks3 + dc4 * ks4 + dc5 * ks5 + dc6 * ks6)

  ! error evaluation
  where( domain_slo_idx .eq. 0 ) hs_err = 0
  errmax = maxval( hs_err ) / eps

  if(errmax.gt.1 .and. ddt .ge. ddt_min_slo) then
   ! try smaller ddt
   ddt = max( safety * ddt * (errmax ** pshrnk), 0.5d0 * ddt )
   ddt_chk_slo = ddt
   !write(*,*) "shrink (slo): ", ddt, errmax, maxloc( hs_err )
   if(ddt.eq.0) stop 'stepsize underflow'
   go to 3
  else
   ! "time + ddt" should be less than "t * dt"
   if(time + ddt .gt. t * dt ) ddt = t * dt - time
   time = time + ddt
   hs_idx = hs_temp
   qs_ave_idx = qs_ave_idx + qs_ave_temp_idx
  endif
  qs_ave_idx = qs_ave_idx / dble(dt) / 6.d0

  ! cumulative rainfall
  do i = 1, ny
   do j = 1, nx
    if( domain(i,j) .ne. 0 ) rain_sum = rain_sum + dble(qp_t(i, j) * area * ddt)
   enddo
  enddo

  if(time.ge.t * dt) exit ! finish for this timestep
 enddo

 time = t * dt

 !******* Evapotranspiration *****************************
 if( evp_switch .ne. 0 ) call evp(hs_idx)

 !******* GROUND WATER ***********************************
 !call gw(hs_idx)
 !call sub_slo_idx2ij( hg_idx, hg )

 ! hs_idx -> hs
 call sub_slo_idx2ij( hs_idx, hs )
 call sub_slo_idx2ij4( qs_ave_idx, qs_ave )

 !******* LEVEE BREAK ************************************
 !call levee_break(t, hr, hs, xllcorner, yllcorner, cellsize)

 !******* RIVER-SLOPE INTERACTIONS ***********************
 if( riv_thresh .ge. 0 ) call funcrs(hr, hs)

 !******* INFILTRATION (Green Ampt) **********************
 call infilt(hs)

 !******* SET WATER DEPTH 0 AT DOMAIN = 2 ****************
 do i = 1, ny
  do j = 1, nx
   if( domain(i,j) .eq. 2  ) then
    sout = sout + hs(i,j) * area
    if( riv(i,j) .eq. 1 ) sout = sout + hr(i,j) * area * area_ratio(i, j)

    hs(i,j) = 0.d0
    if( riv(i,j) .eq. 1 ) hr(i,j) = 0.d0

   endif
  enddo
 enddo

 ! hs -> hs_idx, hr -> hr_idx
 call sub_riv_ij2idx( hr, hr_idx )
 call sub_slo_ij2idx( hs, hs_idx )

 !write(*,*) "max hr: ", maxval(hr), "loc : ", maxloc(hr)
 !write(*,*) "max hs: ", maxval(hs), "loc : ", maxloc(hs)

 !******* OUTPUT *****************************************

 ! open output files
 if( t .eq. out_next ) then

  write(*,*) "OUTPUT :", t, "/", maxt, time

  tt = tt + 1
  out_next = nint((tt+1) * out_dt)
  call int2char(tt, t_char)

  where(domain .eq. 0) hs = -0.1d0
  if(riv_thresh.ge.0) where(domain .eq. 0) hr = -0.1d0
  if(riv_thresh.ge.0) where(domain .eq. 0) qr_ave = -0.1d0 / area
  where(domain .eq. 0) gampt_ff = -0.1d0
  where(domain .eq. 0) aevp = -0.1d0
  where(domain .eq. 0) qe_t = -0.1d0
  where(domain .eq. 0) hg = -0.1d0

  if(outswitch_hs .eq. 1) ofile_hs = trim(outfile_hs) // trim(t_char) // ".out"
  if(outswitch_hs .eq. 2) ofile_hs = trim(outfile_hs) // trim(t_char) // ".bin"
  if(outswitch_hr .eq. 1) ofile_hr = trim(outfile_hr) // trim(t_char) // ".out"
  if(outswitch_hr .eq. 2) ofile_hr = trim(outfile_hr) // trim(t_char) // ".bin"
  if(outswitch_qr .eq. 1) ofile_qr = trim(outfile_qr) // trim(t_char) // ".out"
  if(outswitch_qr .eq. 2) ofile_qr = trim(outfile_qr) // trim(t_char) // ".bin"
  if(outswitch_qu .eq. 1) ofile_qu = trim(outfile_qu) // trim(t_char) // ".out"
  if(outswitch_qu .eq. 2) ofile_qu = trim(outfile_qu) // trim(t_char) // ".bin"
  if(outswitch_qv .eq. 1) ofile_qv = trim(outfile_qv) // trim(t_char) // ".out"
  if(outswitch_qv .eq. 2) ofile_qv = trim(outfile_qv) // trim(t_char) // ".bin"
  if(outswitch_gampt_ff .eq. 1) ofile_gampt_ff = trim(outfile_gampt_ff) // trim(t_char) // ".out"
  if(outswitch_gampt_ff .eq. 2) ofile_gampt_ff = trim(outfile_gampt_ff) // trim(t_char) // ".bin"

  if(outswitch_hs .eq. 1) open( 100, file = ofile_hs )
  if(outswitch_hr .eq. 1) open( 101, file = ofile_hr )
  if(outswitch_qr .eq. 1) open( 102, file = ofile_qr )
  if(outswitch_qu .eq. 1) open( 103, file = ofile_qu )
  if(outswitch_qv .eq. 1) open( 104, file = ofile_qv )
  if(outswitch_gampt_ff .eq. 1) open( 105, file = ofile_gampt_ff )

  if(outswitch_hs .eq. 2) open( 100, file = ofile_hs, form = 'unformatted', access = 'direct', recl = nx*ny*4 )
  if(outswitch_hr .eq. 2) open( 101, file = ofile_hr, form = 'unformatted', access = 'direct', recl = nx*ny*4 )
  if(outswitch_qr .eq. 2) open( 102, file = ofile_qr, form = 'unformatted', access = 'direct', recl = nx*ny*4 )
  if(outswitch_qu .eq. 2) open( 103, file = ofile_qu, form = 'unformatted', access = 'direct', recl = nx*ny*4 )
  if(outswitch_qv .eq. 2) open( 104, file = ofile_qv, form = 'unformatted', access = 'direct', recl = nx*ny*4 )
  if(outswitch_gampt_ff .eq. 2) open( 105, file = ofile_gampt_ff, form = 'unformatted', access = 'direct', recl = nx*ny*4 )

  ! output (ascii)
  do i = 1, ny
   if(outswitch_hs .eq. 1) write(100,'(10000f14.5)') (hs(i, j), j = 1, nx)
   if(outswitch_hr .eq. 1) write(101,'(10000f14.5)') (hr(i, j), j = 1, nx)
   if(outswitch_qr .eq. 1) write(102,'(10000f14.5)') ((qr_ave(i,j) * area), j = 1, nx) ! [m3/s]
   if(outswitch_qu .eq. 1) write(103,'(10000f14.5)') (((qs_ave(1, i, j) + (qs_ave(3, i, j) - qs_ave(4, i, j)) / 2.d0) * area), &
       j = 1, nx)
   if(outswitch_qv .eq. 1) write(104,'(10000f14.5)') (((qs_ave(2, i, j) + (qs_ave(3, i, j) + qs_ave(4, i, j)) / 2.d0) * area), &
       j = 1, nx)
   if(outswitch_gampt_ff .eq. 1) write(105,'(10000f14.5)') (gampt_ff(i, j), j = 1, nx)
  enddo

  ! output (binary)
  if(outswitch_hs .eq. 2) write(100,rec=1) ((hs(i,j), j = 1, nx), i = ny, 1, -1)
  if(outswitch_hr .eq. 2) write(101,rec=1) ((hr(i,j), j = 1, nx), i = ny, 1, -1)
  if(outswitch_qr .eq. 2) write(102,rec=1) ((qr_ave(i,j) * area, j = 1, nx), i = ny, 1, -1) ! [m3/s]
  if(outswitch_qu .eq. 2) write(103,rec=1) (((qs_ave(1, i, j) + (qs_ave(3, i, j) - qs_ave(4, i, j)) / 2.d0) * area), i = ny, 1, -1)
  if(outswitch_qv .eq. 2) write(104,rec=1) (((qs_ave(2, i, j) + (qs_ave(3, i, j) + qs_ave(4, i, j)) / 2.d0) * area), i = ny, 1, -1)
  if(outswitch_gampt_ff .eq. 2) write(105,rec=1) ((gampt_ff(i,j), j = 1, nx), i = ny, 1, -1)

  if(outswitch_hs .ne. 0) close(100)
  if(outswitch_hr .ne. 0) close(101)
  if(outswitch_qr .ne. 0) close(102)
  if(outswitch_qu .ne. 0) close(103)
  if(outswitch_qv .ne. 0) close(104)
  if(outswitch_gampt_ff .ne. 0) close(105)

  if( tec_switch .eq. 1 ) then
   if (tt .eq. 1) then
    call Tecout_alloc(nx, ny, 4)
    call Tecout_mkGrid(dx, dy, zs)
    call Tecout_write_initialize(tt, width, depth, height, area_ratio)
   endif
   call Tecout_write(tt, qp_t, hr, qr_ave, hs, area)
  end if

  ! For TSAS Output
  !call RRI_TSAS(tt, hs_idx, hr_idx, qs_ave_idx, qr_ave_idx, qp_t_idx)

  ! For dt_check
  !call dt_check_riv(hr_idx, tt, ddt_chk_riv)
  !call dt_check_slo(hs_idx, tt, ddt_chk_slo)

  if( dam_switch .eq. 1 ) then
   if( tt .eq. 1 ) open(1001, file = "./out/dam_out.txt")
   call dam_write
  endif

 endif

 ! check water balance
 if(mod(t, 1).eq.0) then
  call storage_calc(hs, hr, ss, sr, si, sg)
  !write(*, '(6e15.3)') rain_sum, pevp_sum, aevp_sum, sout, ss + sr + si + sg, (rain_sum - aevp_sum - sout - (ss + sr + si + sg) &
  !     - sinit)
  write(1000, '(1000e15.7)') rain_sum, pevp_sum, aevp_sum, sout, ss + sr + si + sg, (rain_sum - aevp_sum - sout - (ss + sr + si + &
       sg) - sinit), ss, sr, si, sg
 endif

enddo

!pause

end program RRI
