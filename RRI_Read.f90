subroutine RRI_Read

use globals
use dam_mod
use tecout_mod
implicit none

integer i
character*256 format_version

open(1, file = "RRI_Input.txt", status = 'old')

read(1,'(a)') format_version
write(*,'("format_version : ", a)') trim(adjustl(format_version))

if( format_version .ne. "RRI_Input_Format_Ver1_3" ) stop "This RRI model requires RRI_Input_Format_Ver1_3"

read(1,*)
write(*,*)

read(1,'(a)') rainfile
read(1,'(a)') demfile
read(1,'(a)') accfile
read(1,'(a)') dirfile

write(*,'("rainfile : ", a)') trim(adjustl(rainfile))
write(*,'("demfile : ", a)') trim(adjustl(demfile))
write(*,'("accfile : ", a)') trim(adjustl(accfile))
write(*,'("dirfile : ", a)') trim(adjustl(dirfile))

read(1,*)
write(*,*)

read(1,*) utm
read(1,*) eight_dir
read(1,*) lasth
read(1,*) dt
read(1,*) dt_riv
read(1,*) outnum
read(1,*) xllcorner_rain
read(1,*) yllcorner_rain
read(1,*) cellsize_rain_x, cellsize_rain_y

write(*,'("utm : ", i5)') utm
write(*,'("eight_dir : ", i5)') eight_dir
write(*,'("lasth : ", i8)') lasth
write(*,'("dt : ", i12)') dt
write(*,'("dt_riv : ", i8)') dt_riv
write(*,'("outnum : ", i8)') outnum
write(*,'("xllcorner_rain : ", f15.5)') xllcorner_rain
write(*,'("yllcorner_rain : ", f15.5)') yllcorner_rain
write(*,'("cellsize_rain_x : ", f15.5, "  cellsize_rain_y : ", f15.5)') cellsize_rain_x, cellsize_rain_y

read(1,*)
write(*,*)

read(1,*) num_of_landuse
allocate( dif(num_of_landuse) )
allocate( dm(num_of_landuse), da(num_of_landuse) )
allocate( beta(num_of_landuse), ka(num_of_landuse) )
allocate( soildepth(num_of_landuse), ns_slope(num_of_landuse) )

read(1,*) (dif(i), i = 1, num_of_landuse)
read(1,*) (dm(i), i = 1, num_of_landuse)
read(1,*) (da(i), i = 1, num_of_landuse)
read(1,*) (ka(i), i = 1, num_of_landuse)
read(1,*) (beta(i), i = 1, num_of_landuse)
read(1,*) (soildepth(i), i = 1, num_of_landuse)
read(1,*) (ns_slope(i), i = 1, num_of_landuse)
read(1,*) ns_river

write(*,'("num_of_landuse : ", i5)') num_of_landuse
write(*,'("dif : ", 100i5)') (dif(i), i = 1, num_of_landuse)
write(*,'("dm : ", 100f12.3)') (dm(i), i = 1, num_of_landuse)
write(*,'("da : ", 100f12.3)') (da(i), i = 1, num_of_landuse)
write(*,'("ka : ", 100f12.3)') (ka(i), i = 1, num_of_landuse)
write(*,'("beta : ", 100f12.3)') (beta(i), i = 1, num_of_landuse)
write(*,'("soildepth : ", 100f12.3)') (soildepth(i), i = 1, num_of_landuse)
write(*,'("ns_slope : ", 100f12.3)') (ns_slope(i), i = 1, num_of_landuse)
write(*,'("ns_river : ", f12.3)') ns_river

read(1,*)
write(*,*)

allocate( ksv_1d(num_of_landuse), delta_1d(num_of_landuse), faif_1d(num_of_landuse), infilt_limit_1d(num_of_landuse) )

read(1,*) (ksv_1d(i), i = 1, num_of_landuse)
read(1,*) (delta_1d(i), i = 1, num_of_landuse)
read(1,*) (faif_1d(i), i = 1, num_of_landuse)
read(1,*) (infilt_limit_1d(i), i = 1, num_of_landuse)

write(*,'("ksv : ", 100f15.5)') (ksv_1d(i), i = 1, num_of_landuse)
write(*,'("delta : ", 100f12.3)') (delta_1d(i), i = 1, num_of_landuse)
write(*,'("faif : ", 100f12.3)') (faif_1d(i), i = 1, num_of_landuse)
write(*,'("infilt_limit : ", 100f12.3)') (infilt_limit_1d(i), i = 1, num_of_landuse)

read(1,*)
write(*,*)

read(1,*) riv_thresh
read(1,*) width_param_c
read(1,*) width_param_s
read(1,*) depth_param_c
read(1,*) depth_param_s
read(1,*) height_param
read(1,*) height_limit_param

read(1,*)
write(*,*)

read(1,*) rivfile_switch
read(1,'(a)') widthfile
read(1,'(a)') depthfile
read(1,'(a)') heightfile

if(rivfile_switch.eq.0) then
 write(*,'("riv_thresh : ", i7)') riv_thresh
 write(*,'("width_param_c : ", f12.2)') width_param_c
 write(*,'("width_param_s : ", f12.2)') width_param_s
 write(*,'("depth_param_c : ", f12.2)') depth_param_c
 write(*,'("depth_param_s : ", f12.2)') depth_param_s
 write(*,'("height_param : ", f12.2)') height_param
 write(*,'("height_limit_param : ", i10)') height_limit_param
else
 write(*,'("widthfile : ", a)') trim(adjustl(widthfile))
 write(*,'("depthfile : ", a)') trim(adjustl(depthfile))
 write(*,'("heightfile : ", a)') trim(adjustl(heightfile))
endif

read(1,*)
write(*,*)

read(1,*) init_cond_slo
read(1,*) init_cond_riv

write(*,'("init_cond_slo : ", f7.2)') init_cond_slo
write(*,'("init_cond_riv : ", f7.2)') init_cond_riv

read(1,*)
write(*,*)

read(1,*) init_slo_switch, init_riv_switch, init_gampt_ff_switch
read(1,"(a)") initfile_slo
read(1,'(a)') initfile_riv
read(1,'(a)') initfile_gampt_ff

if(init_slo_switch.ne.0) write(*,'("initfile_slo : ", a)') trim(adjustl(initfile_slo))
if(init_riv_switch.ne.0) write(*,'("initfile_riv : ", a)') trim(adjustl(initfile_riv))
if(init_gampt_ff_switch.ne.0) write(*,'("initfile_gampt_ff : ", a)') trim(adjustl(initfile_gampt_ff))

read(1,*)
write(*,*)

read(1,*) bound_slo_wlev_switch, bound_riv_wlev_switch
read(1,'(a)') boundfile_slo_wlev
read(1,'(a)') boundfile_riv_wlev

if(bound_slo_wlev_switch.ne.0) write(*,'("boundfile_slo_wlev : ", a)') trim(adjustl(boundfile_slo_wlev))
if(bound_riv_wlev_switch.ne.0) write(*,'("boundfile_riv_wlev : ", a)') trim(adjustl(boundfile_riv_wlev))

read(1,*)
write(*,*)

read(1,*) bound_slo_disc_switch, bound_riv_disc_switch
read(1,'(a)') boundfile_slo_disc
read(1,'(a)') boundfile_riv_disc

if(bound_slo_disc_switch.ne.0) write(*,'("boundfile_slo_disc : ", a)') trim(adjustl(boundfile_slo_disc))
if(bound_riv_disc_switch.ne.0) write(*,'("boundfile_riv_disc : ", a)') trim(adjustl(boundfile_riv_disc))

read(1,*)
write(*,*)

read(1,*) land_switch
read(1,'(a)') landfile
if(land_switch.eq.1) write(*,'("landfile : ", a)') trim(adjustl(landfile))

read(1,*)
write(*,*)

read(1,*) dam_switch
read(1,'(a)') damfile
if(dam_switch.eq.1) write(*,'("damfile : ", a)') trim(adjustl(damfile))

read(1,*)
write(*,*)

read(1,*) div_switch
read(1,'(a)') divfile
if(div_switch.eq.1) write(*,'("divfile : ", a)') trim(adjustl(divfile))

read(1,*)
write(*,*)

read(1,*) evp_switch
read(1,'(a)') evpfile
read(1,*) xllcorner_evp
read(1,*) yllcorner_evp
read(1,*) cellsize_evp_x, cellsize_evp_y

if( evp_switch .ne. 0 ) then
 write(*,'("evpfile : ", a)') trim(adjustl(evpfile))
 write(*,'("xllcorner_evp : ", f15.5)') xllcorner_evp
 write(*,'("yllcorner_evp : ", f15.5)') yllcorner_evp
 write(*,'("cellsize_evp_x : ", f15.5, " cellsize_evp_y : ", f15.5)') cellsize_evp_x, cellsize_evp_y
endif

read(1,*)
write(*,*)

!read(1,*) emb_switch
!read(1,'(a)') embrfile
!read(1,'(a)') embbfile
!if(emb_switch.eq.1) write(*,'("embrfile : ", a)') trim(adjustl(embrfile))
!if(emb_switch.eq.1) write(*,'("embbfile : ", a)') trim(adjustl(embbfile))

!read(1,*)
!write(*,*)

read(1,*) outswitch_hs, outswitch_hr, outswitch_qr, outswitch_qu, outswitch_qv, outswitch_gampt_ff, outswitch_storage
read(1,'(a)') outfile_hs
read(1,'(a)') outfile_hr
read(1,'(a)') outfile_qr
read(1,'(a)') outfile_qu
read(1,'(a)') outfile_qv
read(1,'(a)') outfile_gampt_ff
read(1,'(a)') outfile_storage

if(outswitch_hs .ne. 0) write(*,'("outfile_hs : ", a)') trim(adjustl(outfile_hs))
if(outswitch_hr .ne. 0) write(*,'("outfile_hr : ", a)') trim(adjustl(outfile_hr))
if(outswitch_qr .ne. 0) write(*,'("outfile_qr : ", a)') trim(adjustl(outfile_qr))
if(outswitch_qu .ne. 0) write(*,'("outfile_qu : ", a)') trim(adjustl(outfile_qu))
if(outswitch_qv .ne. 0) write(*,'("outfile_qv : ", a)') trim(adjustl(outfile_qv))
if(outswitch_gampt_ff .ne. 0) write(*,'("outfile_gampt_ff : ", a)') trim(adjustl(outfile_gampt_ff))
if(outswitch_storage .ne. 0) write(*,'("outfile_storage : ", a)') trim(adjustl(outfile_storage))

read(1,*)
write(*,*)

read(1,*) tec_switch
read(1,'(a)') tecfile
if(tec_switch.eq.1) write(*,'("tecfile : ", a)') trim(adjustl(tecfile))

end subroutine RRI_Read
