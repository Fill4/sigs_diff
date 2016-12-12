!----------------------------------------------------------------------------
! Joao Faria: Jan 2013	|	Revised: Filipe Pereira - Abr 2016
!----------------------------------------------------------------------------
elemental real(dp) function fun (nu_d2)
! This is the theoretical function to be fitted

	use types_and_interfaces, only: dp
	use commonarray, only: c
	use commonvar, only: w0ref, pi
	implicit none

	real(dp), intent(in)  :: nu_d2
	real(dp) :: xarg, yarg, bcz, heii, factor
	
	! BCZ signal
	!   c(1) = A_bcz
	!   c(2) = tau_bcz
	!   c(3) = phi_bcz

	yarg = 4.0d0*pi*c(2)*nu_d2*w0ref*1.0d-6 + 2.0_dp*c(3)
	bcz  = ( c(1)/nu_d2**2 ) * sin(yarg)

	! HeIIZ
	!   c(4) = A_he
	!   c(5) = beta
	!   c(6) = tau_he
	!   c(7) = phi_he
	
	xarg = 4.0_dp*pi*c(6)*nu_d2*w0ref*1.0d-6 + 2.0_dp*c(7)
	heii = (c(4)*nu_d2*exp(-c(5)*nu_d2**2)) * sin(xarg)

	fun = bcz + heii

	return

end function fun

subroutine rescale(array_in, array_out)
! Rescales the parameters that come out of pikaia ARRAY_IN to their physical values ARRAY_OUT
	
	use types_and_interfaces, only: dp
	use commonvar
	
	implicit none
	
	real, dimension(:), intent(in)			:: array_in
	real(dp), dimension(:), intent(out) 	:: array_out

	
	! Bcz
	array_out(1) = dble(array_in(1)) * 0.50_dp
	array_out(2) = dble(array_in(2)) * (upper_tau_bcz - lower_tau_bcz) + lower_tau_bcz
	array_out(3) = dble(array_in(3)) * pi
	
	! HeII
	array_out(4) = dble(array_in(4)) * (50.0_dp - 0.3_dp) + 0.3_dp
	array_out(5) = dble(array_in(5)) * 5.0_dp
	array_out(6) = dble(array_in(6)) * (upper_tau_he2 - lower_tau_he2) + lower_tau_he2
	array_out(7) = dble(array_in(7)) * pi
  
end subroutine rescale