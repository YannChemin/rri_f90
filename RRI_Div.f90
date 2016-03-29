subroutine RRI_Div(qr_idx, qr_div_idx)
use globals
implicit none

real(8) qr_idx(riv_count), qr_div_idx(riv_count)

integer l, k, kk, kk_div

do l = 1, div_id_max

 k = div_org_idx(l)
 kk = down_riv_idx(k)
 kk_div = div_dest_idx(l)

 if( l.eq.1 ) then
  if( qr_idx(k) .gt. 0.d0 ) then
   qr_div_idx(k) = qr_idx(k) * 0.1
   qr_idx(k) = qr_idx(k) - qr_div_idx(k)
  endif
  elseif( l.eq.2 ) then
   if( qr_idx(k) .gt. 0.d0 ) then
    qr_div_idx(k) = qr_idx(k) * 0.05
    qr_idx(k) = qr_idx(k) - qr_div_idx(k)
   endif
  elseif( l.eq.3 ) then
   if( qr_idx(k) .gt. 0.d0 ) then
    qr_div_idx(k) = qr_idx(k) * 0.30
    qr_idx(k) = qr_idx(k) - qr_div_idx(k)
  endif
 endif
enddo

end subroutine RRI_Div
