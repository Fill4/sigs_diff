!**********************************************************
  elemental real(dp) function fun (w_d2)
!	 this is the function to be fitted.

        use types_and_interfaces, only: dp
        use commonvar, only: pi, pi_sq, w0ref, tau0_houdek, tau0_houdek_sq, nu0
        use commonarray, only: c, polyc

		implicit none
	
		real(dp), intent(in)  :: w_d2

		real(dp) :: xarg, factor, bcz, heiiz
		real(dp) :: poly
		
		
		! w_d2 is (cyclic) frequency nu in Hz 
		
		! third degree polynomial in nu
		!   polyc
!		poly = polyc(1) + polyc(2)*w_d2 + polyc(3)*w_d2**2 + polyc(4)*w_d2**3
        poly = polyc(1) + polyc(2)/w_d2 + polyc(3)/(w_d2**2) + polyc(4)/(w_d2**3)
        
        !poly = c(1) + c(9)*w_d2 + c(10)*w_d2**2 + c(11)*w_d2**3
		!poly = c(1) + c(9)/w_d2 + c(10)/(w_d2**2) + c(11)/(w_d2**3)
		
		
		! BCZ signal
		!   c(2) = tau_c
		!   c(3) = phi_c
		!   c(1) = A_c
		xarg = 2.0d0 * ( 2.d0*pi*c(2)*w_d2 + c(3) )
		factor = 1.0_dp / sqrt(1.0_dp + 0.0625_dp/(pi_sq*tau0_houdek_sq*w_d2*w_d2))
	  	bcz  = ( c(1) * (nu0**3) / (w_d2**2) ) * &
	  	       factor * &
	  	       cos(xarg + atan(4.0_dp*pi*tau0_houdek*w_d2))
	  	
	  	
	  	! HeIIZ
	  	!   c(7) = tau_he
	  	!   c(8) = phi_he
	  	xarg = 2.0d0 * ( 2.d0*pi*c(6)*w_d2 + c(7) )
	  	heiiz = (c(4) * w_d2 * exp(-8.0_dp*pi_sq*(c(5)**2)*(w_d2**2))) * &
	  	        cos(xarg)


	  	
	  	fun = poly + bcz + heiiz

		
  end function fun



















