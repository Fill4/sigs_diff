!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
elemental real(dp) function fun (nu_d2)
! This is the theoretical function to be fitted

	use types_and_interfaces, only: dp
	use commonvar
	use commonarray
	implicit none

	real(dp), intent(in)  :: nu_d2
	real(dp) :: xarg, yarg, bcz, heii, factor
	
	! HeIIZ
	!   c(4) = A_he
	!   c(5) = beta
	!   c(6) = tau_he
	!   c(7) = phi_he
	
	xarg = 4.0_dp*pi*c(6)*w0ref*nu_d2*1d-6 + 2.0_dp*c(7)
	heii = (c(4) * nu_d2 * exp(-(c(5)*2.0_dp*pi*w0ref*1d-6) * nu_d2**2)) * sin(xarg)

	! BCZ signal
	!   c(1) = A_bcz
	!   c(2) = tau_bcz
	!   c(3) = phi_bcz

	yarg = 4.0d0*pi*c(2)*w0ref*nu_d2*1d-6 + 2.0_dp*c(3)
	bcz  = ( c(1) / (nu_d2**2) ) * sin(yarg)

	fun = bcz + heii

	return

end function fun



















