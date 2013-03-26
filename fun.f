!**********************************************************
  elemental real(dp) function fun (w_d2)
!	 this is the function to be fitted. Expression from Mazumdar 2012 (M12)

        use types_and_interfaces, only: dp
        use commonvar, only: pi, pi_sq, tau0_houdek, tau0_houdek_sq, nu0, w0
        use commonarray, only: c, polyc

		implicit none
	
		real(dp), intent(in)  :: w_d2

		real(dp) :: xarg, factor, bcz, heiiz
		real(dp) :: a1, a2, b1, b2, alpha1, alpha2, delta, d, dd
		real(dp) :: poly
		
		
		! w_d2 is (cyclic) frequency nu in Hz 
		
		

		
		! polynomial in nu (can be up to second degree)
		!   polyc
        poly = polyc(1) !+ polyc(2)*w_d2 + polyc(3)*(w_d2**2)

	  	
	  	! HeIIZ
	  	!   c(4) = A_II
	  	!   c(5) = Delta_II
	  	!   c(6) = tau_he
	  	!   c(7) = phi_he
	  	delta = atan( (a1*sin(alpha1)) / &
	  	              (1.0_dp - ((1.0_dp + b1) * cos(alpha1))) &
	  	            )
	  	xarg = 2.0_dp * ( 2.0_dp*pi*c(6)*w_d2 + c(7) ) - delta

	  	heiiz = (c(4) * w_d2 * exp(-8.0_dp*pi_sq*(c(5)**2)*(w_d2**2))) * &
	  	        cos(xarg)
	  	        
	  	        
		! BCZ signal
		!   c(1) = A_c (b2 from M12)
		!   c(2) = tau_c (tau_bcz from M12)
		!   c(3) = phi_c (phi_bcz from M12)

		xarg = 2.0d0 * ( 2.d0*pi*c(2)*w_d2 + c(3) ) - delta
		factor = 1.0_dp / sqrt(1.0_dp + 0.0625_dp/(pi_sq*tau0_houdek_sq*w_d2*w_d2))
	  	bcz  = ( c(1) * (nu0**3) / (w_d2**2) ) * &
	  	       factor * &
	  	       cos(xarg + atan(4.0_dp*pi*tau0_houdek*w_d2))
	  	



	  	
	  	fun = poly + bcz + heiiz

		
  end function fun



















