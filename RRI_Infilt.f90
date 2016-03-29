! infiltration
subroutine infilt(hs)
use globals
implicit none

real(8) hs(ny, nx), gampt_ff_temp(ny, nx)

gampt_f = 0.d0
gampt_ff_temp = gampt_ff
where( gampt_ff_temp .le. 0.01d0 ) gampt_ff_temp = 0.01d0

! gampt_f : infiltration capacity [m/s]
! gampt_ff : accumulated infiltration depth [m]
gampt_f = ksv * (1.d0 + faif * delta / gampt_ff_temp)

! gampt_f : infiltration capacity -> infiltration rate [m/s]
where( gampt_f .ge. hs / dt ) gampt_f = hs / dt

! gampt_ff should not exceeds a certain level
where( infilt_limit .ge. 0.d0 .and. gampt_ff .ge. infilt_limit ) gampt_f = 0.d0

! update gampt_ff [m]
gampt_ff = gampt_ff + gampt_f * dt

! hs : hs - infiltration rate * dt [m]
hs = hs - gampt_f * dt
where( hs .le. 0.d0 ) hs = 0.d0

end subroutine infilt
