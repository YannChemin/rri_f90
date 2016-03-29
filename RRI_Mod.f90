module globals

character*256 rainfile
character*256 demfile
character*256 accfile
character*256 dirfile

integer rivfile_switch
character*256 widthfile
character*256 depthfile
character*256 heightfile

integer init_slo_switch, init_riv_switch, init_gampt_ff_switch
character*256 initfile_slo
character*256 initfile_riv
character*256 initfile_gampt_ff

integer bound_slo_wlev_switch, bound_riv_wlev_switch
character*256 boundfile_slo_wlev
character*256 boundfile_riv_wlev

integer bound_slo_disc_switch, bound_riv_disc_switch
character*256 boundfile_slo_disc
character*256 boundfile_riv_disc

integer land_switch
character*256 landfile

integer div_switch
character*256 divfile

integer evp_switch
character*256 evpfile

!integer emb_switch
!character*256 embrfile, embbfile

integer outswitch_hs
integer outswitch_hr
integer outswitch_qr
integer outswitch_qu
integer outswitch_qv
integer outswitch_gampt_ff
integer outswitch_storage

character*256 outfile_hs
character*256 outfile_hr
character*256 outfile_qr
character*256 outfile_qu
character*256 outfile_qv
character*256 outfile_gampt_ff
character*256 outfile_storage

integer lasth
integer dt
integer dt_riv
integer outnum
real(8) xllcorner_rain
real(8) yllcorner_rain
real(8) cellsize_rain_x, cellsize_rain_y

real(8) init_cond_slo
real(8) init_cond_riv
integer utm
integer eight_dir

integer, save :: num_of_landuse
integer, allocatable, save :: dif(:)
real(8), allocatable, save :: dm(:)
real(8), allocatable, save :: da(:)
real(8), allocatable, save :: ka(:)
real(8), allocatable, save :: beta(:)
real(8), allocatable, save :: soildepth(:)
real(8), allocatable, save :: ns_slope(:)
real(8), save :: ns_river

real(8), allocatable, save :: ksv_1d(:), ksv(:,:)
real(8), allocatable, save :: delta_1d(:), delta(:,:)
real(8), allocatable, save :: faif_1d(:), faif(:,:)
real(8), allocatable, save :: infilt_limit_1d(:), infilt_limit(:,:)

integer riv_thresh
real(8) width_param_c
real(8) width_param_s
real(8) depth_param_c
real(8) depth_param_s
real(8) height_param
integer height_limit_param

integer maxt, div
real(8), save :: ddt

integer, save :: nx, ny
real(8) xllcorner, yllcorner
real(8) cellsize
real(8), save :: length, area, dx, dy
integer i4
parameter( i4 = 4 )

integer, allocatable, save :: domain(:,:), dir(:,:)
real(8), allocatable, save :: zs(:,:), zb(:,:), zb_riv(:,:)
integer, allocatable, save :: riv(:,:), acc(:,:), land(:,:)
real(8), allocatable, save :: width(:,:), depth(:,:), height(:,:), area_ratio(:,:)
real(8), allocatable, save :: bound_slo_wlev(:,:), bound_riv_wlev(:,:)
real(8), allocatable, save :: bound_slo_disc(:,:), bound_riv_disc(:,:)

real(8), save :: time
integer, save :: tt_max_bound_slo_wlev, tt_max_bound_riv_wlev
integer, save :: tt_max_bound_slo_disc, tt_max_bound_riv_disc
integer, allocatable :: t_bound_slo_wlev(:), t_bound_riv_wlev(:)
integer, allocatable :: t_bound_slo_disc(:), t_bound_riv_disc(:)
real(8), allocatable, save :: gampt_ff(:,:), gampt_f(:,:)

integer, save :: riv_count
integer, allocatable, save :: riv_idx2i(:), riv_idx2j(:), riv_ij2idx(:,:)
integer, allocatable, save :: down_riv_idx(:), domain_riv_idx(:)
real(8), allocatable, save :: width_idx(:), depth_idx(:), height_idx(:), area_ratio_idx(:)
real(8), allocatable, save :: zb_riv_idx(:), dis_riv_idx(:)
real(8), allocatable, save :: bound_slo_wlev_idx(:,:), bound_riv_wlev_idx(:,:)
real(8), allocatable, save :: bound_slo_disc_idx(:,:), bound_riv_disc_idx(:,:)
integer, allocatable, save :: dif_riv_idx(:)

integer, save :: lmax, slo_count
integer, allocatable, save :: slo_idx2i(:), slo_idx2j(:), slo_ij2idx(:,:)
integer, allocatable, save :: down_slo_idx(:,:), domain_slo_idx(:), land_idx(:)
integer, allocatable, save :: down_slo_1d_idx(:)
real(8), allocatable, save :: ns_slo_idx(:), ka_idx(:), da_idx(:), dm_idx(:), beta_idx(:), soildepth_idx(:)
real(8), allocatable, save :: zb_slo_idx(:), dis_slo_idx(:,:), len_slo_idx(:,:)
real(8), allocatable, save :: dis_slo_1d_idx(:), len_slo_1d_idx(:)
integer, allocatable, save :: dif_slo_idx(:)

character*256 ofile_hs, ofile_hr, ofile_qr, ofile_qu, ofile_qv, ofile_gampt_ff

integer, save :: id_break

integer, save :: div_id_max
integer, allocatable, save :: div_org_idx(:), div_dest_idx(:)

real(8), allocatable, save :: hg(:,:), hg_idx(:)

real(8), save :: xllcorner_evp, yllcorner_evp
real(8), save :: cellsize_evp_x, cellsize_evp_y

integer, allocatable, save :: evp_i(:), evp_j(:)
integer, save :: tt_max_evp
integer, allocatable, save :: t_evp(:)
integer, save :: nx_evp, ny_evp
real(8), allocatable, save :: qe(:,:,:), qe_t(:,:), qe_t_idx(:), aevp(:,:)
real(8), save :: aevp_sum, pevp_sum

real(8), allocatable, save :: qrs(:,:)
integer, allocatable, save :: hs_id(:,:), hr_id(:,:)
integer, allocatable, save :: hs_id_idx(:), hr_id_idx(:)
real(8), allocatable, save :: aevp_tsas(:)

!real(8), allocatable, save :: emb_r(:,:), emb_b(:,:)
!real(8), allocatable, save :: emb_r_idx(:), emb_b_idx(:)

end module globals
