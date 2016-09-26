elemental real(dp) function smooth_comp (nu_d2)
! Polynomial smooth function. Depends on degree defined.

	use types_and_interfaces, only: dp
	use commonvar, only: degree, w0ref
	use commonarray, only: polyc
	implicit none

	real(dp), intent(in)	:: nu_d2
	integer					:: i
	smooth_comp = 0

	do i=0,degree
		smooth_comp = smooth_comp + (polyc(i+1)/nu_d2**i)
	end do

end function smooth_comp

elemental real(dp) function he_comp (nu_d2)
! Function that represents the signal produced by the sharp transition in 
! the helium ionization region.

	use types_and_interfaces, only: dp
	use commonvar, only: pi, w0ref
	use commonarray, only: c
	implicit none

	real(dp), intent(in)  :: nu_d2
	real(dp) :: xarg
	
	! HeIIZ
	!   c(4) = A_he
	!   c(5) = beta
	!   c(6) = tau_he
	!   c(7) = phi_he
	
	xarg = 4.0_dp*pi*c(6)*nu_d2*w0ref*1.0d-6 + 2.0_dp*c(7)
	he_comp = (c(4)*nu_d2*exp(-c(5)*nu_d2**2)) * sin(xarg)

end function he_comp

elemental real(dp) function bcz_comp (nu_d2)
! Function that represents the signal produced by the sharp transition in 
! the base of the convection zone.

	use types_and_interfaces, only: dp
	use commonvar, only: pi, w0ref
	use commonarray, only: c
	implicit none

	real(dp), intent(in)  :: nu_d2	
	real(dp) :: yarg
	
	! BCZ signal
	!   c(1) = A_bcz
	!   c(2) = tau_bcz
	!   c(3) = phi_bcz
	
	yarg =  4.0d0*pi*c(2)*nu_d2*w0ref*1.0d-6 + 2.0_dp*c(3)
	bcz_comp  = ( c(1)/nu_d2**2 ) * sin(yarg)

end function bcz_comp