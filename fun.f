!**********************************************************
  elemental real(dp) function fun (w_d2)
!	 this is the function to be fitted.

        use types_and_interfaces, only: dp
        use commonvar, only: pi, pi_sq, w0ref, tau0_houdek, tau0_houdek_sq, nu0
        use commonarray, only: c, nd2

		implicit none
	
		real(dp), intent(in)  :: w_d2

		real(dp) :: xarg, factor, bcz, heiiz
		real(dp) :: poly
		
		
		! w_d2 is (cyclic) frequency nu in Hz 
		
		! third degree polynomial in nu^-1
		!   c(1), c(9), c(10), c(11)    
        poly = c(1) + c(9)*w_d2 + c(10)*w_d2**2 + c(11)*w_d2**3
		!poly = c(1) + c(9)/w_d2 + c(10)/(w_d2**2) + c(11)/(w_d2**3)
		
		
		! BCZ signal
		!   c(3) = tau_c
		!   c(4) = phi_c
		!   c(2) = A_c
		xarg = 2.0d0 * ( 2.d0*pi*c(3)*w_d2 + c(4) )
		factor = 1.0_dp / sqrt(1.0_dp + 0.0625_dp*pi_sq*tau0_houdek_sq*w_d2*w_d2)
	  	bcz  = ( c(2) * (nu0**3) / (w_d2**2) ) * &
	  	       factor * &
	  	       cos(xarg + atan(4.0_dp*pi*tau0_houdek*w_d2))
	  	
	  	
	  	! HeIIZ
	  	!   c(7) = tau_he
	  	!   c(8) = phi_he
	  	xarg = 2.0d0 * ( 2.d0*pi*c(7)*w_d2 + c(8) )
	  	heiiz = (c(5) * w_d2 * exp(-8.0_dp*pi_sq*(c(6)**2)*(w_d2**2))) * &
	  	        cos(xarg)


	  	
	  	fun = poly + bcz + heiiz

		
  end function fun



















